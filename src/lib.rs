pub mod ecs;
pub mod enum_map;
pub mod math;
pub mod transform;
pub mod w2ss;

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
    use bevy::platform::collections::HashMap;
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
