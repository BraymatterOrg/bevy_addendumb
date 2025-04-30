use std::{
    any::TypeId,
    f32::consts::TAU,
    ops::{Add, Div, Mul, Sub},
    time::Duration,
};

use bevy::{math::VectorSpace, prelude::*, utils::TypeIdMap};
use rand::Rng;

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

pub struct EaseScatter<S: Scatter> {
    pub scatter: S,
    pub ease: EaseFunction,
}

impl<S: Scatter> Scatter for EaseScatter<S> {
    type SampleParam = S::SampleParam;

    fn init(&self, rng: &mut impl Rng) -> Self::SampleParam {
        self.scatter.init(rng)
    }

    fn sample(&self, mut ctx: SampleCtx<Self::SampleParam>, rng: &mut impl Rng) -> Vec2 {
        // `sample_unchecked` doesn't panic, it just returns values with no guarantees outside of
        // [0, 1], which is okay because we know we're in [0, 1]
        ctx.progress = EasingCurve::new(0., 1., self.ease).sample_unchecked(ctx.progress);
        self.scatter.sample(ctx, rng)
    }
}

/// Outer radius is 1. Scale with the `scale` param in `scatter`.
pub struct DiskScatter {
    pub inner_radius: f32,
}

impl Scatter for DiskScatter {
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

pub fn get_xy_plane_intersection(ray_origin: Vec3, ray_direction: Vec3) -> Option<Vec3> {
    // Make sure the ray isn't parallel to the plane (i.e., Dz shouldn't be zero)
    if ray_direction.z.abs() < f32::EPSILON {
        return None; // No intersection or ray is parallel to XY plane
    }

    // Solve for t where Pz = 0
    let t = -ray_origin.z / ray_direction.z;

    // We typically only care about intersections in the direction the ray is pointing
    if t < 0.0 {
        return None; // Intersection is "behind" the ray
    }

    // Final intersection point
    let intersection = ray_origin + t * ray_direction;
    Some(intersection)
}
