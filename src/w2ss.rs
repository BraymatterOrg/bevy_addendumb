use std::marker::PhantomData;
use bevy::{prelude::*, ecs::system::SystemParam, reflect::Reflect, render::primitives::Aabb};

#[derive(Default)]
pub struct W2ssPlugin<CameraMarker: Component + Default + TypePath, A: AabbAlternative + Component>
{
    pub _camera_marker: PhantomData<CameraMarker>,
    pub _aabb_alternative: PhantomData<A>,
}

impl<CameraMarker: Component + Default + TypePath, A: AabbAlternative + Component> Plugin
    for W2ssPlugin<CameraMarker, A>
{
    fn build(&self, app: &mut App) {
        app.add_systems(PreUpdate, (update_world_to_screenspace::<CameraMarker, A>,));
        app.register_type::<WorldToScreenspace<CameraMarker>>();
    }
}

#[derive(Component, Debug, Reflect, Clone, Default)]
#[reflect(Component)]
pub struct WorldToScreenspace<CameraMarker: Component + Default> {
    pub radius: Option<f32>,
    pub calc_method: RadiusCalcMethod,
    pub recalculate_radius: bool,
    pub allow_negative_z: bool,

    pub screen_position: Option<Vec2>,
    pub screen_radius: Option<f32>,
    pub screen_dims: Option<Vec2>,

    #[reflect(ignore)]
    pub _camera_marker: PhantomData<CameraMarker>,
}

impl<C: Component + Default> WorldToScreenspace<C> {
    pub fn new<A: AabbAlternative + Component>(
        transform: &GlobalTransform,
        entity: Entity,
        param: &WorldToScreenspaceParam<C, A>,
    ) -> Self {
        let mut world_to_screenspace = Self::default();
        world_to_screenspace.update(transform, entity, param);
        world_to_screenspace
    }

    pub fn update<A: AabbAlternative + Component>(
        &mut self,
        transform: &GlobalTransform,
        entity: Entity,
        param: &WorldToScreenspaceParam<C, A>,
    ) {
        let WorldToScreenspaceParam {
            camera_query,
            children,
            aabb_alternative_query,
            aabb_query,
        } = param;

        let Ok((camera, cam_gtsf)) = camera_query.single() else {
            debug!("Could not get camera for WorldToScreenSpace calculations.");
            return;
        };

        let world_to_viewport = |world_to_ss: &mut WorldToScreenspace<C>, transform: Vec3| {
            if world_to_ss.allow_negative_z {
                // same as world_to_viewport but allowing ndc_space_coords.z to be over 1
                world_to_ss.screen_dims.and_then(|target_size| {
                    camera
                        .world_to_ndc(cam_gtsf, transform)
                        .map(|ndc_space_coords| {
                            let mut ndc_space_coords_2 = ndc_space_coords.truncate();
                            if ndc_space_coords.z < 0. {
                                ndc_space_coords_2 = -ndc_space_coords_2;
                            }
                            let mut viewport_position =
                                (ndc_space_coords_2 + Vec2::ONE) / 2.0 * target_size;
                            viewport_position.y = target_size.y - viewport_position.y;
                            viewport_position
                        })
                })
            } else {
                camera.world_to_viewport(cam_gtsf, transform).ok()
            }
        };

        let ent_tsf = transform.compute_transform();
        self.screen_dims = camera.logical_viewport_size();
        self.screen_position = world_to_viewport(self, transform.translation());

        if self.radius.is_none() || self.recalculate_radius {
            let aabb = std::iter::once(entity)
                .chain(children.iter_descendants(entity))
                .flat_map(|e| {
                    aabb_alternative_query
                        .get(e)
                        .map(|alternative| alternative.aabb())
                        .or_else(|_| aabb_query.get(e))
                        .ok()
                })
                .next();

            // If there's no AABB skip processing
            let Some(aabb) = aabb else {
                return;
            };

            let vec3aabb: Vec3 = aabb.half_extents.into();
            let scaled_extents = vec3aabb * ent_tsf.scale;

            self.radius = Some(match self.calc_method {
                RadiusCalcMethod::Min => scaled_extents.min_element(),
                RadiusCalcMethod::Max => scaled_extents.max_element(),
                RadiusCalcMethod::Avg => {
                    (scaled_extents[0] + scaled_extents[1] + scaled_extents[2]) / 3.0
                }
                RadiusCalcMethod::X => scaled_extents.x,
                RadiusCalcMethod::Y => scaled_extents.y,
                RadiusCalcMethod::Z => scaled_extents.z,
            });
        }

        let Some(screen_pos) = self.screen_position else {
            self.screen_radius = None;
            return;
        };

        let world_position =
            transform.translation() + Vec3::new(self.radius.unwrap_or(0.), 0.0, 0.0);
        let Some(screen_pos_extents) = world_to_viewport(self, world_position) else {
            self.screen_radius = None;
            return;
        };

        self.screen_radius = Some(screen_pos_extents.distance(screen_pos));
    }

    ///Returns an Option as to whether the object is considered on_screen, with Some returning the screen dimensions and position of the object on screen
    pub fn on_screen(&self) -> Option<(Vec2, Vec2)> {
        if self.screen_dims.is_some() && self.screen_position.is_some() {
            Some((self.screen_dims.unwrap(), self.screen_position.unwrap()))
        } else {
            None
        }
    }
}

pub trait AabbAlternative {
    fn aabb(&self) -> &Aabb;
}

impl AabbAlternative for Aabb {
    fn aabb(&self) -> &Aabb {
        self
    }
}

/// Used in conjunction with the AabbSizedUI component to determine how the radius of the UI element is calculated, based on the AABB extents of the entity
#[derive(Default, Reflect, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RadiusCalcMethod {
    /// Will take the minimum of the half extents of the first AABB found
    Min,
    /// Will take the maximum of the half extents of the first AABB found
    Max,
    /// Averages the extents of the first AABB found
    #[default]
    Avg,
    /// Use the X extent of the first AABB found
    X,
    /// Use the Y extent of the first AABB found
    Y,
    /// Use the Z extent of the first AABB found
    Z,
}

#[derive(SystemParam)]
pub struct WorldToScreenspaceParam<
    'w,
    's,
    CameraMarker: Component + Default,
    A: AabbAlternative + Component,
> {
    camera_query: Query<'w, 's, (&'static Camera, &'static GlobalTransform), With<CameraMarker>>,
    children: Query<'w, 's, &'static Children>,
    aabb_alternative_query: Query<'w, 's, &'static A>,
    aabb_query: Query<'w, 's, &'static Aabb>,
}

pub fn update_world_to_screenspace<
    CameraMarker: Component + Default,
    A: AabbAlternative + Component,
>(
    mut world_to_ss_query: Query<(
        Entity,
        &mut WorldToScreenspace<CameraMarker>,
        &GlobalTransform,
    )>,
    param: WorldToScreenspaceParam<CameraMarker, A>,
) {
    world_to_ss_query
        .iter_mut()
        .for_each(|(entity, mut world_to_ss, transform)| {
            world_to_ss.update(transform, entity, &param);
        });
}
