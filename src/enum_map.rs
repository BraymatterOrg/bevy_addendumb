use std::fmt::Debug;
use std::fmt::{self, Formatter};
use std::hash::Hash;

use bevy::platform::collections::hash_map::{Iter, IterMut};
use bevy::{platform::collections::HashMap, reflect::Reflect};
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

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
