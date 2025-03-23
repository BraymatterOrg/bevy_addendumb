use std::time::Duration;

use bevy::{ecs::{component::{ComponentHooks, StorageType}, system::QueryLens}, prelude::*};

use crate::math::ExpDecay;

/// Target for `MoveTo`
#[derive(Clone, Copy)]
pub enum TransformTarget {
    Point(Transform),
    Entity(Entity),
    EntityOffset(Entity, Transform),
}

impl From<Vec3> for TransformTarget {
    fn from(value: Vec3) -> Self {
        TransformTarget::Point(Transform::from_translation(value))
    }
}

impl From<Entity> for TransformTarget {
    fn from(value: Entity) -> Self {
        TransformTarget::Entity(value)
    }
}

impl TransformTarget {
    pub fn position(self, mut gtsfs_query: QueryLens<&GlobalTransform>) -> Option<GlobalTransform> {
        match self {
            TransformTarget::Point(target) => Some(GlobalTransform::from(target)),
            TransformTarget::Entity(target) => {
                gtsfs_query
                .query()
                .get(target)
                .ok().copied()
            },
            TransformTarget::EntityOffset(ent, offset) => {
                    let query = gtsfs_query.query();
                    let Ok(tgt) = query.get(ent) else{
                        warn!("Could not get entity for Transform Target");
                        return None;
                    };


                    let mut tgt = tgt.compute_transform();
                    tgt.translation += offset.translation;
                    tgt.rotation *= offset.rotation;

                    Some(GlobalTransform::from(tgt))
            }
        }
    }

    /// Intended to be used in places where a query cannot be passed in. Relies on the caller to provide the relevant transforms
    pub fn calculate(self, target_tsf: Option<GlobalTransform>, target_ent_tsf: Option<GlobalTransform>) -> Option<GlobalTransform> {
        match self {
            TransformTarget::Point(transform) => {
                Some(GlobalTransform::from(transform))
            },
            TransformTarget::Entity(_entity) => {
                let Some(gtsf) = target_ent_tsf else {
                    return None;
                };

                Some(gtsf)
            },
            TransformTarget::EntityOffset(_, _) => {
                let Some(ent_gtsf) = target_ent_tsf else {
                    return None;
                };

                let Some(target_gtsf) = target_tsf else {
                    return None;
                };

                let ent_tsf = ent_gtsf.compute_transform();
                let mut tsf = target_gtsf.compute_transform();
                tsf.translation += ent_tsf.translation;
                tsf.rotation *= ent_tsf.rotation;

                Some(GlobalTransform::from(tsf))
            },
        }
    }
}

/// Moves an entity to a target along a hermite spline. If `start_vel` equals `end_vel` and they
/// have the same direction as the displacement between the entity and its target, it moves in a
/// straight line.
#[derive(Clone)]
pub struct FollowSpline {
    /// Initialized the first time `move_toward_entity` runs
    start_pos: Option<Vec3>,
    start_vel: Vec3,

    /// The point to move to
    target: TransformTarget,
    /// Affects how sharply the entity changes path to follow a moving target
    path_correction: f32,
    /// A position which the entity moves to, which smoothly lerps to the target position
    aimed_pos: Option<Vec3>,
    end_vel: Vec3,

    vel: Vec3,
    ///Remove Component after timer expires. Should be true by default, but for things like elemental missile it can be false so it always 'finds' the targeted entity/location
    auto_remove: bool,
    timer: Timer,
}

impl FollowSpline {
    pub fn new(
        start_vel: Vec3,
        target: impl Into<TransformTarget>,
        end_vel: Vec3,
        duration: Duration,
        path_correction: f32,
        auto_remove: bool
    ) -> Self {
        Self {
            start_pos: None,
            start_vel,
            target: target.into(),
            path_correction,
            aimed_pos: None,
            end_vel,
            vel: start_vel,
            auto_remove,
            timer: Timer::new(duration, TimerMode::Once),
        }
    }
}

//TODO: Remove this garbage. It should never have been in a component hook. Deferred world doesn't allow queries etc. 
//Which leads to hacky workarounds like below. 
impl Component for FollowSpline {
    const STORAGE_TYPE: StorageType = StorageType::Table;

    fn register_component_hooks(hooks: &mut ComponentHooks) {
        hooks.on_insert(|mut world, entity, _| {
            let move_to = world
                .get::<FollowSpline>(entity)
                .expect("`FollowSpline` is missing in its `on_insert` hook");

            let (target_tsf, ent_tsf) = match move_to.target {
                TransformTarget::Point(transform) => {
                    (Some(GlobalTransform::from(transform)), None)
                },
                TransformTarget::Entity(tgt_ent) => {
                    let Some(ent_tsf) = world.get::<GlobalTransform>(tgt_ent) else {
                        warn!("Could not find ent transform in component hook");
                        return;
                    };
                    (None, Some(*ent_tsf))
                },
                TransformTarget::EntityOffset(tgt_ent, transform) => {
                    let Some(ent_tsf) = world.get::<GlobalTransform>(tgt_ent) else {
                        warn!("Could not find ent transform in component hook");
                        return;
                    };

                    (Some(GlobalTransform::from(transform)), Some(*ent_tsf))
                },
            };

            let target_pos = move_to.target.calculate(target_tsf, ent_tsf);

            if let Some(target_pos) = target_pos {
                world
                    .get_mut::<FollowSpline>(entity)
                    .expect("`MoveTo` is missing even though we just had it")
                    .aimed_pos = Some(target_pos.translation());
            }
        });
    }
}

pub fn move_to(
    mut tsf_query: Query<&GlobalTransform>,
    mut mover_query: Query<(&mut FollowSpline, &mut Transform, Entity)>,
    mut cmds: Commands,
    time: Res<Time>,
) {
    for (mut mover, mut mover_tsf, mover_ent) in mover_query.iter_mut() {
        if let Some(target) = mover.target.position(tsf_query.as_query_lens()) {
            // The target still exists
            let path_correction = mover.path_correction;
            let aimed_pos = mover.aimed_pos.get_or_insert(target.translation());
            *aimed_pos = aimed_pos.exp_decay(target.translation(), path_correction, time.delta());
        }

        let Some(aimed_pos) = mover.aimed_pos else {
            // The target didn't exist when `MoveTo` was inserted, so just move at the initial
            // velocity. If the game despawns out of bounds entities (distance check), the entity
            // will leave bounds and be despawned.
            mover_tsf.translation += mover.vel * time.delta_secs();
            continue;
        };

        let start_pos = *mover.start_pos.get_or_insert(mover_tsf.translation);

        let curve =
            match CubicHermite::new([start_pos, aimed_pos], [mover.start_vel, mover.end_vel])
                .to_curve()
            {
                Ok(curve) => curve,
                Err(err) => {
                    error!("couldn't create a hermite curve for `MoveTo`: {err}");
                    continue;
                }
            };

        mover.timer.tick(time.delta());
        let t = mover.timer.fraction();
        mover_tsf.translation = curve.position(t);

        let vel = curve.velocity(t);
        mover.vel = vel;
        mover_tsf.look_to(vel, Vec3::Y);

        //If the timer's expired remove this component
        if t >= 1.0 && mover.auto_remove {
            let Some(mut entcmds) = cmds.get_entity(mover_ent) else {
                warn!("Could not find entcmds to remove follow spline from");
                return;
            };
            
            entcmds.remove::<FollowSpline>();
        }
    }
}