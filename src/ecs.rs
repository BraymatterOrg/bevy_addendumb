use std::{any::{type_name, Any}, collections::VecDeque, marker::PhantomData, time::Duration};

use bevy::{asset::LoadState, ecs::{query::{QueryData, QueryFilter, WorldQuery}, system::SystemId}, pbr::{ExtendedMaterial, MaterialExtension}, prelude::*, render::render_resource::{AsBindGroup, ShaderRef}};
use leafwing_input_manager::{prelude::ActionState, Actionlike};

use crate::transform::move_to;

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

impl DespawnAfterCommandsExt for Commands<'_, '_> {
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

///Despawns all entities with the specified component immediately
pub fn despawn_ents_with_component<T: Component>(mut cmds: Commands, query: Query<Entity, With<T>>){
    query.iter().for_each(|ent|{
        if let Some(entcmds) = cmds.get_entity(ent) {
            entcmds.despawn_recursive();
        }
    })
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

//TODO: Feature gate this and leafwing-input-manager
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

//TODO: This could just be remove component after...?
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