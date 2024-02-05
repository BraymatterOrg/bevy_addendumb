use bevy::ecs::query::{ReadOnlyWorldQuery, WorldQuery};
use bevy::ecs::system::EntityCommand;
use bevy::utils::hashbrown::hash_map::{Iter, IterMut};
use bevy::utils::HashMap;
use bevy::{ecs::system::Command, prelude::*};
use leafwing_input_manager::action_state::ActionState;
use leafwing_input_manager::Actionlike;
use std::any::{type_name, Any};
use std::collections::VecDeque;
use std::fmt::{self, Debug, Formatter};
use std::hash::Hash;
use std::{marker::PhantomData, time::Duration};
use strum::IntoEnumIterator;

pub struct EcsAddendumPlugin;

impl Plugin for EcsAddendumPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<DespawnAfter>();
        app.add_systems(
            PreUpdate,
            (goodbye_system.after(to_batman), to_batman, despawn_after),
        );
        app.add_systems(
            Update,
            (despawn_on_key, despawn_on_gamepad_button, visible_after),
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

pub struct InsertTimedComponent<T: Any + Component> {
    pub proc: T,
    pub duration: Duration,
}

impl<T: Any + Component> EntityCommand for InsertTimedComponent<T> {
    fn apply(self, id: Entity, world: &mut World) {
        let has_t = world.entity(id).contains::<T>();

        if !has_t {
            world
                .entity_mut(id)
                .insert((self.proc, RemoveComponentAfter::<T>::new(self.duration)));

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
    ents_to_add.for_each(|(instance, ent)| {
        let Some(mut entcmds) = cmds.get_entity(ent) else {
            warn!("Could not get entity for adding a material instance");
            return;
        };

        entcmds.insert(mats.add(instance.instance.clone()));
        entcmds.remove::<AddMaterialInstance<T>>();
    });
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
        let Some(mut entref) = world.get_entity_mut(self.target) else {
            warn!("Could not find entity for DespawnAfterCommand");
            return;
        };

        entref.insert(DespawnAfter::from(self.duration));
    }
}

impl<'w, 's> DespawnAfterCommandsExt for Commands<'w, 's> {
    fn despawn_after<T: Into<DespawnAfterCommand>>(&mut self, despawn_after: T) {
        self.add(despawn_after.into());
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
    departures.for_each(|d| {
        let Some(entcmds) = cmds.get_entity(d) else {
            warn!("Could not get entity for goodbye system");
            return;
        };

        entcmds.despawn_recursive();
    });
}

pub fn to_batman(bruces: Query<Entity, With<ToBatman>>, mut cmds: Commands) {
    bruces.for_each(|orphan_of_destiny| {
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
            if action_state.just_released(despawn_on_action.0) {
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
    input: Res<Input<KeyCode>>,
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
pub struct DespawnOnGamepadButtonRelease(pub GamepadButtonType);

impl Default for DespawnOnGamepadButtonRelease {
    fn default() -> Self {
        Self(GamepadButtonType::Start)
    }
}

fn despawn_on_gamepad_button(
    mut cmds: Commands,
    mut query: Query<(Entity, &DespawnOnGamepadButtonRelease)>,
    gamepads: Res<Gamepads>,
    input: Res<Input<GamepadButton>>,
) {
    query.iter_mut().for_each(|(ent, despawn_on_button)| {
        for gamepad in gamepads.iter() {
            let button = GamepadButton {
                gamepad,
                button_type: despawn_on_button.0,
            };
            if input.just_released(button) {
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
    F: ReadOnlyWorldQuery,
    Q: WorldQuery,
{
    type ReadOnlyReturn = <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'w>;
    type MutReturn = <Q as WorldQuery>::Item<'w>;

    fn get_or_nearest_ancestor(
        &'w self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> Option<Self::ReadOnlyReturn> {
        std::iter::once(entity)
            .chain(parent_query.iter_ancestors(entity))
            .find_map(|e| self.get(e).ok())
    }

    fn get_or_nearest_ancestor_mut(
        &'w mut self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> Option<Self::MutReturn> {
        let matched = std::iter::once(entity)
            .chain(parent_query.iter_ancestors(entity))
            .find(|e| self.contains(*e));
        matched.and_then(|e| self.get_mut(e).ok())
    }

    fn contains_including_ancestors(
        &'w self,
        entity: Entity,
        parent_query: &Query<&Parent>,
    ) -> bool {
        std::iter::once(entity)
            .chain(parent_query.iter_ancestors(entity))
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

#[derive(Reflect, Eq, PartialEq, Clone)]
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
