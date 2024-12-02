use bevy::asset::LoadState;
use bevy::ecs::component::{ComponentHooks, StorageType};
use bevy::ecs::query::{QueryData, QueryFilter, WorldQuery};
use bevy::ecs::system::{EntityCommand, QueryLens, SystemId, SystemParam};
use bevy::math::VectorSpace;
use bevy::pbr::{ExtendedMaterial, MaterialExtension};
use bevy::render::primitives::Aabb;
use bevy::render::render_resource::{AsBindGroup, ShaderRef};
use bevy::utils::hashbrown::hash_map::{Iter, IterMut};
use bevy::utils::{HashMap, TypeIdMap};
use bevy::{ecs::world::Command, prelude::*};
use leafwing_input_manager::action_state::ActionState;
use leafwing_input_manager::Actionlike;
use rand::Rng;
use serde::{Deserialize, Serialize};
use std::any::{type_name, Any, TypeId};
use std::collections::VecDeque;
use std::f32::consts::TAU;
use std::fmt::{self, Debug, Formatter};
use std::hash::Hash;
use std::ops::{Add, Div, Mul, Sub};
use std::{marker::PhantomData, time::Duration};
use strum::IntoEnumIterator;

pub struct EcsAddendumPlugin;

impl Plugin for EcsAddendumPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(MaterialPlugin::<
            ExtendedMaterial<StandardMaterial, ScrollExtension>,
        >::default());
        app.register_type::<DespawnAfter>();
        app.add_systems(
            PreUpdate,
            (goodbye_system.after(to_batman), to_batman, despawn_after),
        );
        app.add_systems(
            Update,
            (
                despawn_on_key,
                despawn_on_gamepad_button,
                visible_after,
                await_asset,
                move_to,
            ),
        );
        app.world_mut().resource_mut::<Assets<Shader>>().insert(
            SCROLL_SHADER_HANDLE.id(),
            Shader::from_wgsl(include_str!("scroll.wgsl"), "scroll.wgsl"),
        );
    }
}
///Used to indicate that a child should have its parents... removed.
#[derive(Component)]
pub struct ToBatman;

///Used to indicate that this entity needs to be... departed.
#[derive(Component)]
pub struct Goodbye;

#[derive(Component, Default)]
pub struct RemoveComponentAfter<T: Component> {
    pub timer: Timer,
    pub t: PhantomData<T>,
}

impl<T: Component> RemoveComponentAfter<T> {
    #[allow(dead_code)] //TODO
    pub fn new(duration: Duration) -> Self {
        Self {
            timer: Timer::from_seconds(duration.as_secs_f32(), TimerMode::Once),
            t: PhantomData,
        }
    }
}

pub fn remove_component_after<T: Component>(
    mut q: Query<(Entity, &mut RemoveComponentAfter<T>)>,
    mut cmds: Commands,
    time: Res<Time>,
) {
    q.iter_mut().for_each(|(ent, mut remove_component)| {
        remove_component.timer.tick(time.delta());

        if remove_component.timer.finished() {
            let Some(mut entcmds) = cmds.get_entity(ent) else {
                warn!("Could not get entity for removing a component");
                return;
            };

            entcmds.remove::<T>();
            entcmds.remove::<RemoveComponentAfter<T>>();
        }
    });
}

/// Inserts the component and keeps it for the given duration, and then removes it. Doesn't replace
/// existing timed components, but extends the duration if it is greater than the current
/// component's remaining duration.
pub struct InsertTimedComponent<T: Any + Component> {
    pub component: T,
    pub duration: Duration,
}

impl<T: Any + Component> EntityCommand for InsertTimedComponent<T> {
    fn apply(self, id: Entity, world: &mut World) {
        let has_t = world.entity(id).contains::<T>();

        if !has_t {
            world.entity_mut(id).insert((
                self.component,
                RemoveComponentAfter::<T>::new(self.duration),
            ));

            return;
        }

        let mut entity = world.entity_mut(id);
        let Some(mut remove) = entity.get_mut::<RemoveComponentAfter<T>>() else {
            warn!(
                "Timed component `{}` is missing `RemoveComponentAfter`",
                type_name::<T>()
            );

            return;
        };

        // Use the longest duration
        if remove.timer.remaining() >= self.duration {
            return;
        }

        remove.timer = Timer::new(self.duration, TimerMode::Once);
    }
}

#[derive(Component, Default)]
pub struct AddMaterialInstance<T: Material> {
    pub instance: T,
}

impl<T: Material> AddMaterialInstance<T> {
    pub fn new(instance: T) -> Self {
        Self { instance }
    }
}

pub fn add_material_instance<T: Material>(
    mut mats: ResMut<Assets<T>>,
    mut cmds: Commands,
    ents_to_add: Query<(&AddMaterialInstance<T>, Entity), Added<AddMaterialInstance<T>>>,
) {
    ents_to_add.iter().for_each(|(instance, ent)| {
        let Some(mut entcmds) = cmds.get_entity(ent) else {
            warn!("Could not get entity for adding a material instance");
            return;
        };

        entcmds.insert(MeshMaterial3d(mats.add(instance.instance.clone())));
        entcmds.remove::<AddMaterialInstance<T>>();
    });
}

#[derive(AsBindGroup, Asset, Clone, TypePath)]
pub struct ScrollExtension {
    #[uniform(100)]
    pub scroll: Vec2,
}

const SCROLL_SHADER_HANDLE: Handle<Shader> =
    Handle::weak_from_u128(123669721584892194424305348291015135349);

impl MaterialExtension for ScrollExtension {
    fn fragment_shader() -> ShaderRef {
        SCROLL_SHADER_HANDLE.into()
    }
}

#[derive(Component, Default, Reflect)]
#[reflect(Component)]
pub struct DespawnAfter {
    pub timer: Timer,
    pub recursive: bool,
    /// If true, the timer will only tick when the entity is visible.
    /// Useful for despawning entities that are spawned invisible and wait for something to
    /// trigger them being visible.
    pub only_when_visible: bool,
}

impl DespawnAfter {
    pub fn recursive(mut self) -> Self {
        self.recursive = true;
        self
    }

    pub fn new(duration: Duration, recursive: bool, only_when_visible: bool) -> Self {
        Self {
            timer: Timer::new(duration, TimerMode::Once),
            recursive,
            only_when_visible,
        }
    }
    pub fn only_when_visible(mut self) -> Self {
        self.only_when_visible = true;
        self
    }
}

pub struct DespawnAfterCommand {
    pub duration: Duration,
    pub target: Entity,
    pub recursive: bool,
}

impl From<Duration> for DespawnAfter {
    fn from(value: Duration) -> Self {
        DespawnAfter {
            timer: Timer::from_seconds(value.as_secs_f32(), TimerMode::Once),
            recursive: false,
            only_when_visible: false,
        }
    }
}

pub trait DespawnAfterCommandsExt {
    fn despawn_after<T: Into<DespawnAfterCommand>>(&mut self, despawn_after: T);
}

impl Command for DespawnAfterCommand {
    fn apply(self, world: &mut World) {
        let Ok(mut entref) = world.get_entity_mut(self.target) else {
            warn!("Could not find entity for DespawnAfterCommand");
            return;
        };

        entref.insert(DespawnAfter::from(self.duration));
    }
}

impl<'w, 's> DespawnAfterCommandsExt for Commands<'w, 's> {
    fn despawn_after<T: Into<DespawnAfterCommand>>(&mut self, despawn_after: T) {
        self.queue(despawn_after.into());
    }
}

pub fn despawn_after(
    mut query: Query<(
        Entity,
        &mut DespawnAfter,
        Option<&InheritedVisibility>,
        Option<&ViewVisibility>,
    )>,
    time: Res<Time>,
    mut cmds: Commands,
) {
    query
        .iter_mut()
        .for_each(|(ent, mut despawn, h_visibility, v_visibility)| {
            if despawn.only_when_visible
                && h_visibility.map(|v| !v.get()).unwrap_or_default()
                && v_visibility.map(|v| !v.get()).unwrap_or_default()
            {
                return;
            }
            despawn.timer.tick(time.delta());

            if despawn.timer.finished() {
                let Some(mut entcmds) = cmds.get_entity(ent) else {
                    warn!("Could not find entity for despawn_after command");
                    return;
                };

                match despawn.recursive {
                    true => entcmds.despawn_recursive(),
                    false => entcmds.despawn(),
                }
            }
        });
}

pub fn goodbye_system(departures: Query<Entity, Added<Goodbye>>, mut cmds: Commands) {
    departures.iter().for_each(|d| {
        let Some(entcmds) = cmds.get_entity(d) else {
            warn!("Could not get entity for goodbye system");
            return;
        };

        entcmds.despawn_recursive();
    });
}

pub fn to_batman(bruces: Query<Entity, With<ToBatman>>, mut cmds: Commands) {
    bruces.iter().for_each(|orphan_of_destiny| {
        let Some(mut entcmds) = cmds.get_entity(orphan_of_destiny) else {
            warn!("Could not get entity for to_batman");
            return;
        };

        entcmds.remove::<ToBatman>();
        entcmds.remove_parent_in_place();
    })
}

#[derive(Component, Reflect, Default)]
#[reflect(Component)]
pub struct DespawnOnAction<T: Actionlike + Default + Clone + Copy + Eq + PartialEq>(pub T);

pub fn despawn_on_action<T: Actionlike + Default + Clone + Copy + Eq + PartialEq>(
    mut cmds: Commands,
    mut query: Query<(Entity, &ActionState<T>, &DespawnOnAction<T>)>,
) {
    query
        .iter_mut()
        .for_each(|(ent, action_state, despawn_on_action)| {
            if action_state.just_released(&despawn_on_action.0) {
                if let Some(entity_commands) = cmds.get_entity(ent) {
                    entity_commands.despawn_recursive()
                }
            }
        });
}

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct DespawnOnKeyRelease(pub KeyCode);

impl Default for DespawnOnKeyRelease {
    fn default() -> Self {
        Self(KeyCode::Space)
    }
}

fn despawn_on_key(
    mut cmds: Commands,
    mut query: Query<(Entity, &DespawnOnKeyRelease)>,
    input: Res<ButtonInput<KeyCode>>,
) {
    query.iter_mut().for_each(|(ent, despawn_on_key)| {
        if input.just_released(despawn_on_key.0) {
            if let Some(entity_commands) = cmds.get_entity(ent) {
                entity_commands.despawn_recursive()
            }
        }
    });
}

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct DespawnOnGamepadButtonRelease(pub GamepadButton);

impl Default for DespawnOnGamepadButtonRelease {
    fn default() -> Self {
        Self(GamepadButton::Start)
    }
}

fn despawn_on_gamepad_button(
    mut cmds: Commands,
    mut query: Query<(Entity, &DespawnOnGamepadButtonRelease)>,
    gamepads_query: Query<&Gamepad>,
) {
    query.iter_mut().for_each(|(ent, despawn_on_button)| {
        for gamepad in gamepads_query.iter() {
            if gamepad.just_released(despawn_on_button.0) {
                if let Some(entity_commands) = cmds.get_entity(ent) {
                    entity_commands.despawn_recursive()
                }
            }
        }
    });
}

/// A component that will set an entity to [`Visibility::Visible`] after the given duration.
/// The duration is affected by pausing the game.
#[derive(Component, Reflect, Default)]
#[reflect(Component)]
pub struct MakeVisibleAfter(Timer);

impl MakeVisibleAfter {
    #[allow(dead_code)]
    pub fn new(duration: Duration) -> Self {
        Self(Timer::new(duration, TimerMode::Once))
    }
}

fn visible_after(
    mut query: Query<(Entity, &mut MakeVisibleAfter)>,
    time: Res<Time>,
    mut cmds: Commands,
) {
    query.iter_mut().for_each(|(ent, mut visible_after)| {
        visible_after.0.tick(time.delta());

        if visible_after.0.finished() {
            if let Some(mut entity_commands) = cmds.get_entity(ent) {
                entity_commands.insert(Visibility::Visible);
                entity_commands.remove::<MakeVisibleAfter>();
            }
        }
    });
}

pub trait FindNearestExt<'w> {
    type ReadOnlyReturn;
    type MutReturn;

    /// Returns the result of the query for the given entity, or the first parent that matches
    /// the query.
    fn get_or_nearest_ancestor(
        &'w self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> Option<Self::ReadOnlyReturn>;

    /// Returns the mutable result of the query for the given entity, or the first parent that
    /// matches the query.
    fn get_or_nearest_ancestor_mut(
        &'w mut self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> Option<Self::MutReturn>;

    /// Returns true if the given entity or any of its ancestors matches the query.
    fn contains_including_ancestors(
        &'w self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> bool;
}

impl<'w, 's, Q, F> FindNearestExt<'w> for Query<'w, 's, Q, F>
where
    's: 'w,
    Q: QueryData,
    F: QueryFilter,
{
    type ReadOnlyReturn = <Q::ReadOnly as WorldQuery>::Item<'w>;
    type MutReturn = <Q as WorldQuery>::Item<'w>;

    fn get_or_nearest_ancestor(
        &'w self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> Option<Self::ReadOnlyReturn> {
        Iterator::chain(std::iter::once(entity), parent_query.iter_ancestors(entity))
            .find_map(|e| self.get(e).ok())
    }

    fn get_or_nearest_ancestor_mut(
        &'w mut self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> Option<Self::MutReturn> {
        let matched = Iterator::chain(std::iter::once(entity), parent_query.iter_ancestors(entity))
            .find(|e| self.contains(*e));
        matched.and_then(|e| self.get_mut(e).ok())
    }

    fn contains_including_ancestors(
        &'w self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> bool {
        Iterator::chain(std::iter::once(entity), parent_query.iter_ancestors(entity))
            .any(|e| self.contains(e))
    }
}

pub trait HierarchyEntityRefExt<'w> {
    /// Returns an [`Iterator`] of [`EntityRef`]s over all of `EntityRef`s descendants.
    ///
    /// Traverses the hierarchy breadth-first.
    fn iter_descendants(self, world: &'w World) -> DescendantIter<'w>;
}

impl<'w> HierarchyEntityRefExt<'w> for EntityRef<'w> {
    fn iter_descendants(self, world: &'w World) -> DescendantIter<'w> {
        DescendantIter::new(self, world)
    }
}

/// An [`Iterator`] of [`EntityRef`]s over the descendants of an [`EntityRef`].
///
/// Traverses the hierarchy breadth-first.
pub struct DescendantIter<'w> {
    world: &'w World,
    vecdeque: VecDeque<Entity>,
}

impl<'w> DescendantIter<'w> {
    pub fn new(entity_ref: EntityRef<'w>, world: &'w World) -> Self {
        DescendantIter {
            world,
            vecdeque: entity_ref
                .get::<Children>()
                .into_iter()
                .flatten()
                .copied()
                .collect(),
        }
    }
}

impl<'w> Iterator for DescendantIter<'w> {
    type Item = EntityRef<'w>;

    fn next(&mut self) -> Option<Self::Item> {
        let entity = self.vecdeque.pop_front()?;
        if let Some(children) = self.world.get::<Children>(entity) {
            self.vecdeque.extend(children);
        }
        Some(self.world.entity(entity))
    }
}

#[derive(Serialize, Deserialize, Reflect, Eq, PartialEq, Clone)]
#[serde(transparent)]
pub struct EnumMap<K: IntoEnumIterator + Eq + PartialEq + Hash, V> {
    map: HashMap<K, V>,
}

impl<K: IntoEnumIterator + Eq + PartialEq + Hash, V: Default> Default for EnumMap<K, V> {
    fn default() -> Self {
        let iter = K::iter();
        let mut map = HashMap::<K, V>::new();

        for enum_value in iter {
            map.insert(enum_value, V::default());
        }

        EnumMap { map }
    }
}

/// Copies the values of the source hashmap into the enum map, will error if the source map is non-exhaustive
/// Returns an Err of the missing element if the source map is not exhaustive
impl<K: IntoEnumIterator + Eq + Hash, V> TryFrom<HashMap<K, V>> for EnumMap<K, V> {
    type Error = K;

    fn try_from(source: HashMap<K, V>) -> Result<Self, Self::Error> {
        for enum_key in K::iter() {
            if !source.contains_key(&enum_key) {
                return Err(enum_key);
            }
        }

        Ok(Self { map: source })
    }
}

impl<K: Debug + IntoEnumIterator + Eq + PartialEq + Hash, V: Debug + Default> Debug
    for EnumMap<K, V>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.map.fmt(f)
    }
}

impl<K: IntoEnumIterator + Eq + PartialEq + Hash, V: Default> EnumMap<K, V> {
    /// Copies the source hashmap to the enum map and backfills anything missing with the default values
    pub fn new(mut source: HashMap<K, V>) -> Self {
        let mut map = HashMap::new();

        for enum_key in K::iter() {
            if let Some(a) = source.remove(&enum_key) {
                map.insert(enum_key, a);
            } else {
                map.insert(enum_key, V::default());
            }
        }

        Self { map }
    }
}

impl<K: IntoEnumIterator + Eq + PartialEq + Hash, V> EnumMap<K, V> {
    pub fn get(&self, key: &K) -> &V {
        self.map.get(key).unwrap()
    }

    pub fn get_mut(&mut self, key: &K) -> &mut V {
        self.map.get_mut(key).unwrap()
    }

    pub fn get_key_value_mut(&mut self, key: &K) -> (&K, &mut V) {
        self.map.get_key_value_mut(key).unwrap()
    }

    pub fn get_key_value(&self, key: &K) -> (&K, &V) {
        self.map.get_key_value(key).unwrap()
    }

    pub fn iter(&self) -> Iter<'_, K, V> {
        self.map.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        self.map.iter_mut()
    }
}

/// Generates a cloneable trait with the given bounds. The bounds must be object-safe. The generated
/// trait has a function `fn dyn_clone(&self) -> Box<dyn GeneratedTrait>` and is blanket impled for
/// types that are bounded by the given bounds and implement `Clone`. This macro is a
/// generalization over workarounds for `Clone` not being object-safe.
#[macro_export]
macro_rules! dyn_clone {
    //pub      MyClone      <
    ($vis:vis $ident:ident$(<
        // Since trailing `+`s aren't allowed in lists of bounds and `$($x)+*` doesn't treat `+` as
        // a separator, we accept the first bound separately from the rest
        // T              :  'static                  +  Send + Sync          ,
        // U
        $($generic:ident$(: $first_generic_bound:tt $(+ $generic_bound:tt)*)?),*
//  >    :  Send             +  Sync
    >)?$(: $first_bound:tt $(+ $bound:tt)*)?) => {
    //   pub trait MyClone <
        $vis trait $ident$(<
            // T        :  'static               +  Send + Sync       ,
            // U
            $($generic$(: $first_generic_bound $(+ $generic_bound)*)?),*
    //  >  : 'static       +  Send + Sync {
        >)?: 'static + $($first_bound $(+ $bound)*)? {
        //  fn dyn_clone(&self) -> Box<dyn  MyClone<   T       , U> >;
            fn dyn_clone(&self) -> Box<dyn $ident$(<$($generic),*>)?>;
    //  }
        }

    //  impl<
        impl<
            //   T        :  'static               +  Send + Sync      ,
            //   U                                                     ,
            $($($generic$(: $first_generic_bound $(+ $generic_bound)*)?,)*)?
            // In macros, we use the `__identifier` convention in contexts where the identifier
            // could conflict with one the user provided
        //  __DynCloneT: 'static + Clone   +  Send          +  Sync
            __DynCloneT: 'static + Clone $(+ $first_bound $(+ $bound)*)?
    //  >  MyClone<   T       , U>  for __DynCloneT {
        > $ident$(<$($generic),*>)? for __DynCloneT {
        //  fn dyn_clone(&self) -> Box<dyn  MyClone<   T       , U> > {
            fn dyn_clone(&self) -> Box<dyn $ident$(<$($generic),*>)?> {
            //  Box::new(self.clone())
                Box::new(self.clone())
        //  }
            }
    //  }
        }
    };
}

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

        let Ok((camera, cam_gtsf)) = camera_query.get_single() else {
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

/// Waits until the given asset is loaded, then runs the given system
#[derive(Component)]
pub struct AwaitAsset {
    pub asset: UntypedHandle,
    pub system: SystemId<In<Entity>>,
}

impl AwaitAsset {
    pub fn new(asset: impl Into<UntypedHandle>, system: SystemId<In<Entity>>) -> Self {
        Self {
            asset: asset.into(),
            system,
        }
    }
}

fn await_asset(
    awaiters_query: Query<(Entity, &AwaitAsset)>,
    assets: Res<AssetServer>,
    mut cmds: Commands,
) {
    for (awaiter, await_asset) in &awaiters_query {
        use LoadState::*;

        let state = assets.load_state(await_asset.asset.id());

        if let Loaded = state {
            cmds.run_system_with_input(await_asset.system, awaiter);
        }

        if let Loaded | Failed(_) = state {
            cmds.entity(awaiter).remove::<AwaitAsset>();
        }
    }
}

#[derive(Debug)]
pub struct Modifier<T> {
    t: T,
    timer: Option<Timer>,
}

impl<T> Modifier<T> {
    pub fn timed(&mut self, duration: Duration) -> &mut Self {
        self.timer = Some(Timer::new(duration, TimerMode::Once));
        self
    }
}

/// Stores modifiers for some data with types as the key. May store modifiers that expire after a
/// duration. Useful for temporary effects that modify stats (ex mana generation rate, damage,
/// speed) where the sources of the modifiers are in completely different parts of the code.
///
/// ```ignore
/// const BASE_MANA_GENERATION_PERIOD: f32 = Duration::from_secs(1);
///
/// let mut modifiers = TypedModifiers::default();
///
/// struct ManaGenTotem;
/// if player_near_mana_gen_totem() {
///     modifiers.push::<ManaGenTotem>(1.5);
/// }
///
/// if player_also_near_other_mana_gen_totem() {
///     // `push` allows multiple values to stack with the same type key
///     modifiers.push::<ManaGenTotem>(1.5);
/// }
///
/// struct ManaGenBuff;
/// if apply_mana_gen_buff() {
///     // `insert` overwrites other values with the same type key. So, the player can only benefit
///     // from the buff once.
///     modifiers.insert::<ManaGenBuff>(2.).timed(Duration::from_secs(2));
/// }
///
/// // A lot of time passed, so the mana buff is removed
/// modifiers.tick(Duration::from_secs(3));
///
/// // This is the period with the modifiers applied
/// let mana_gen_period = modifiers.div_duration(BASE_MANA_GENERATION_PERIOD);
/// ```
#[derive(Default, Debug)]
pub struct TypedModifiers<T = f32>(TypeIdMap<Vec<Modifier<T>>>);

impl<T> TypedModifiers<T> {
    pub fn insert<K: 'static>(&mut self, val: T) -> &mut Modifier<T> {
        let Self(mods) = self;

        let k = TypeId::of::<K>();
        mods.insert(
            k,
            vec![Modifier {
                t: val,
                timer: None,
            }],
        );

        &mut mods
            .get_mut(&k)
            .expect("couldn't get entry that was just inserted")[0]
    }

    /// Insert the value, but stack with other modifiers on the same type instead of replacing them
    pub fn push<K: 'static>(&mut self, val: T) -> &mut Modifier<T> {
        let Self(mods) = self;

        let k = TypeId::of::<K>();
        mods.entry(k).or_default().push(Modifier {
            t: val,
            timer: None,
        });

        mods.get_mut(&k)
            .expect("couldn't get entry that was just inserted")
            .last_mut()
            .expect("couldn't get entry that was just inserted")
    }

    pub fn remove<K: 'static>(&mut self) {
        let Self(mods) = self;
        mods.remove(&TypeId::of::<K>());
    }

    pub fn tick(&mut self, delta: Duration) {
        let Self(mods) = self;

        mods.retain(|_, mods| {
            mods.retain_mut(|modifier| {
                if let Some(ref mut timer) = modifier.timer {
                    timer.tick(delta);
                    !timer.finished()
                } else {
                    true
                }
            });

            !mods.is_empty()
        });
    }
}

impl<T: Add<Output = T> + Clone> TypedModifiers<T> {
    pub fn add(&self, mut val: T) -> T {
        let Self(mods) = self;

        for modifier in mods.values().flatten() {
            val = val + modifier.t.clone();
        }

        val
    }
}

impl<T: Sub<Output = T> + Clone> TypedModifiers<T> {
    pub fn sub(&self, mut val: T) -> T {
        let Self(mods) = self;

        for modifier in mods.values().flatten() {
            val = val - modifier.t.clone();
        }

        val
    }
}

impl<T: Mul<Output = T> + Clone> TypedModifiers<T> {
    pub fn mul(&self, mut val: T) -> T {
        let Self(mods) = self;

        for modifier in mods.values().flatten() {
            val = val * modifier.t.clone();
        }

        val
    }
}

impl<T: Div<Output = T> + Clone> TypedModifiers<T> {
    pub fn div(&self, mut val: T) -> T {
        let Self(mods) = self;

        for modifier in mods.values().flatten() {
            val = val / modifier.t.clone();
        }

        val
    }
}

impl TypedModifiers<f32> {
    pub fn mul_duration(&self, mut val: Duration) -> Duration {
        let Self(mods) = self;

        for modifier in mods.values().flatten() {
            val = val.mul_f32(modifier.t);
        }

        val
    }

    pub fn div_duration(&self, mut val: Duration) -> Duration {
        let Self(mods) = self;

        for modifier in mods.values().flatten() {
            val = val.div_f32(modifier.t);
        }

        val
    }
}

pub trait ExpDecay {
    /// Decays `self` to `b`. Stable in varying framerates. Especially useful in situations where
    /// you don't store the starting value or `b` is moving. Decay values between 1 and 25 tend to
    /// work well.
    fn exp_decay(self, b: Self, decay: f32, dt: Duration) -> Self;
}

impl<T: VectorSpace> ExpDecay for T {
    fn exp_decay(self, b: Self, decay: f32, dt: Duration) -> Self {
        b + (self - b) * (-decay * dt.as_secs_f32()).exp()
    }
}

pub trait ElasticOut {
    /// Performs the elastic out ease function. `a` increases the frequency and somewhat increases
    /// the amplitude. `b` decreases the amplitude and increases the precision near 1. `a` should be
    /// an integer at least 1. If the function must end near 1, `b` should be at least 3. 2 for `a`
    /// and 10 for `b` are good defaults.
    fn elastic_out(self, a: Self, b: Self) -> Self;
}

// Can't impl for a trait-bound generic bc `clamp` isn't from a trait
impl ElasticOut for f32 {
    fn elastic_out(self, a: Self, b: Self) -> Self {
        let clamped = self.clamp(0., 1.);
        -(-a * TAU * (clamped + 1.)).cos() * 2f32.powf(-b * clamped) + 1.
    }
}

/// Target for `MoveTo`
#[derive(Clone, Copy)]
pub enum MoveToTarget {
    Point(Vec3),
    Entity(Entity),
}

impl From<Vec3> for MoveToTarget {
    fn from(value: Vec3) -> Self {
        MoveToTarget::Point(value)
    }
}

impl From<Entity> for MoveToTarget {
    fn from(value: Entity) -> Self {
        MoveToTarget::Entity(value)
    }
}

impl MoveToTarget {
    pub fn position(self, mut gtsfs_query: QueryLens<&GlobalTransform>) -> Option<Vec3> {
        match self {
            MoveToTarget::Point(target) => Some(target),
            MoveToTarget::Entity(target) => gtsfs_query
                .query()
                .get(target)
                .ok()
                .map(GlobalTransform::translation),
        }
    }
}

/// Moves an entity to a target along a hermite spline. If `start_vel` equals `end_vel` and they
/// have the same direction as the displacement between the entity and its target, it moves in a
/// straight line.
pub struct MoveTo {
    /// Initialized the first time `move_toward_entity` runs
    start_pos: Option<Vec3>,
    start_vel: Vec3,

    /// The point to move to
    target: MoveToTarget,
    /// Affects how sharply the entity changes path to follow a moving target
    path_correction: f32,
    /// A position which the entity moves to, which smoothly lerps to the target position
    aimed_pos: Option<Vec3>,
    end_vel: Vec3,

    vel: Vec3,
    timer: Timer,
}

impl MoveTo {
    pub fn new(
        start_vel: Vec3,
        target: impl Into<MoveToTarget>,
        end_vel: Vec3,
        duration: Duration,
        path_correction: f32,
    ) -> Self {
        Self {
            start_pos: None,
            start_vel,
            target: target.into(),
            path_correction,
            aimed_pos: None,
            end_vel,
            vel: start_vel,
            timer: Timer::new(duration, TimerMode::Once),
        }
    }
}

impl Component for MoveTo {
    const STORAGE_TYPE: StorageType = StorageType::Table;

    fn register_component_hooks(hooks: &mut ComponentHooks) {
        hooks.on_insert(|mut world, entity, _| {
            let move_to = world
                .get::<MoveTo>(entity)
                .expect("`MoveTo` is missing in its `on_insert` hook");

            // I don't think it's possible to get a `Query` from a `DeferredWorld` here, so can't
            // reuse `MoveToTarget::position`
            let target_pos = match move_to.target {
                MoveToTarget::Point(target) => Some(target),
                MoveToTarget::Entity(target) => world
                    .get::<GlobalTransform>(target)
                    .map(GlobalTransform::translation),
            };

            if let Some(target_pos) = target_pos {
                world
                    .get_mut::<MoveTo>(entity)
                    .expect("`MoveTo` is missing even though we just had it")
                    .aimed_pos = Some(target_pos);
            }
        });
    }
}

pub fn move_to(
    mut tsf_query: Query<&GlobalTransform>,
    mut mover_query: Query<(&mut MoveTo, &mut Transform)>,
    time: Res<Time>,
) {
    for (mut mover, mut mover_tsf) in mover_query.iter_mut() {
        if let Some(target) = mover.target.position(tsf_query.as_query_lens()) {
            // The target still exists
            let path_correction = mover.path_correction;
            let aimed_pos = mover.aimed_pos.get_or_insert(target);
            *aimed_pos = aimed_pos.exp_decay(target, path_correction, time.delta());
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
    }
}

pub struct SampleCtx<'a, P> {
    pub param: &'a P,
    pub progress: f32,
}

/// Scatters positions onto a plane
pub trait Scatter {
    /// Data needed to sample initialized once per `scatter` call, ex a seed
    type SampleParam;

    fn init(&self, rng: &mut impl Rng) -> Self::SampleParam;
    fn sample(&self, ctx: SampleCtx<Self::SampleParam>, rng: &mut impl Rng) -> Vec2;

    fn scatter(
        &self,
        count: u32,
        center: Vec3,
        normal: Dir3,
        scale: f32,
        rng: &mut impl Rng,
    ) -> impl Iterator<Item = Vec3> {
        let param = self.init(rng);
        let tsf = GlobalTransform::from(
            Transform::from_translation(center)
                .looking_to(normal, Dir3::Y)
                .with_scale(Vec3::splat(scale)),
        );

        (0..count).map(move |i| {
            tsf.transform_point(
                self.sample(
                    SampleCtx {
                        param: &param,
                        progress: i as f32 / count as f32,
                    },
                    rng,
                )
                .extend(0.),
            )
        })
    }
}

pub struct NoScatter;

impl Scatter for NoScatter {
    type SampleParam = ();

    fn init(&self, _: &mut impl Rng) {}

    fn sample(&self, _: SampleCtx<()>, _: &mut impl Rng) -> Vec2 {
        Vec2::ZERO
    }
}

pub struct UniformCircleScatter;

impl Scatter for UniformCircleScatter {
    type SampleParam = ();

    fn init(&self, _: &mut impl Rng) {}

    fn sample(&self, _: SampleCtx<()>, rng: &mut impl Rng) -> Vec2 {
        Vec2::from_angle(rng.gen_range(0. ..TAU)) * rng.gen::<f32>().sqrt()
    }
}

pub struct ExpandingCircleScatter;

impl Scatter for ExpandingCircleScatter {
    type SampleParam = ();

    fn init(&self, _: &mut impl Rng) {}

    fn sample(&self, ctx: SampleCtx<()>, rng: &mut impl Rng) -> Vec2 {
        Vec2::from_angle(rng.gen_range(0. ..TAU)) * ctx.progress.sqrt()
    }
}

/// Outer radius is 1. Scale with the `scale` param in `scatter`.
pub struct AnnulusScatter {
    pub inner_radius: f32,
}

impl Scatter for AnnulusScatter {
    // Inner radius squared. Just cached math.
    type SampleParam = f32;

    fn init(&self, _: &mut impl Rng) -> f32 {
        self.inner_radius * self.inner_radius
    }

    fn sample(&self, ctx: SampleCtx<f32>, rng: &mut impl Rng) -> Vec2 {
        let &inner_radius_squared = ctx.param;
        Vec2::from_angle(rng.gen_range(0. ..TAU))
            * (rng.gen::<f32>() * (1. - inner_radius_squared) + inner_radius_squared)
    }
}

#[cfg(test)]
mod test {
    use bevy::utils::HashMap;
    use strum_macros::EnumIter;

    use super::*;

    #[derive(EnumIter, Eq, PartialEq, Hash)]
    pub enum TestEnum {
        One,
        Two,
        Three,
    }

    #[test]
    pub fn test_enummap() {
        //Test Non-Valid Source errors
        let mut source =
            HashMap::from([(TestEnum::One, 0), (TestEnum::Two, 1), (TestEnum::Three, 2)]);

        //Should be valid
        assert!(EnumMap::try_from(source).is_ok());

        //Should be invalid
        source = HashMap::from([(TestEnum::One, 3)]);
        assert!(EnumMap::try_from(source).is_err());

        //Test Default Backfill
        source = HashMap::from([(TestEnum::One, 2)]);
        let map = EnumMap::new(source);

        //Non default value should be present
        assert!(map.get(&TestEnum::One) == &2);

        //Default Values should be backfilled
        assert!(map.get(&TestEnum::Two) == &0);
        assert!(map.get(&TestEnum::Three) == &0);
    }

    dyn_clone!(CloneClosure<T: Eq + Send + Sync>: (Fn() -> T) + Send + Sync);

    /// This primarily tests for compile errors
    #[test]
    pub fn test_dyn_clone() {
        let my_fn = Box::new(|| 3);
        let my_fn_2 = my_fn.dyn_clone();
        assert_eq!(my_fn(), my_fn_2());
    }
}
