use std::{
    f32::consts::{PI, TAU},
    marker::PhantomData,
    ops::Range,
};

use bevy::{
    ecs::{
        component::{ComponentHooks, StorageType},
        system::{EntityCommand, EntityCommands},
    },
    prelude::*,
    state::state::FreelyMutableState,
    utils::HashSet,
};
use interpolation::{Ease, EaseFunction};
use num::cast::AsPrimitive;
use serde::{Deserialize, Serialize};
use transform::transform_utils_plugin;

pub mod transform;

pub struct UtilPlugin;
impl Plugin for UtilPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(transform_utils_plugin);

        app.init_resource::<BillboardMesh>();
        app.add_systems(Update, billboard);
    }
}

/// Updates entity transforms to orbit around a point, spaced evenly
#[derive(Component, Reflect)]
pub struct OrbitController<T: Component + Default> {
    /// Point about which to orbit on the XZ plane
    pub point: Vec3,

    //Seconds PER REVOLUTION
    pub speed: f32,

    ///Radius of the Orbit (Could maybe be a vec3?)
    pub radius: f32,

    pub phantom_data: PhantomData<T>,
}

impl<T: Component + Default> Default for OrbitController<T> {
    fn default() -> Self {
        Self {
            point: Default::default(),
            speed: 1.0,
            radius: 10.0,
            phantom_data: default(),
        }
    }
}

/// Controlled by an Orbit Controller
#[derive(Component, Default, Reflect)]
pub struct Orbiter<T: Component + Default> {
    phantom_data: PhantomData<T>,
}

pub fn orbit_controllers<T: Component + Default>(
    mut orbit_controllers: Query<(&OrbitController<T>, &Children)>,
    mut orbiters: Query<(&Orbiter<T>, &mut Transform)>,
    time: Res<Time>,
) {
    for (orbit_controller, controller_children) in orbit_controllers.iter_mut() {
        let mut child_orbiters = vec![];

        for child in controller_children.iter() {
            if let Ok(_child_orbiter) = orbiters.get_mut(*child) {
                child_orbiters.push(child);
            }
        }

        for (idx, orbiter) in child_orbiters.iter().enumerate() {
            let Ok((_orbiter, mut tsf)) = orbiters.get_mut(**orbiter) else {
                warn!("Could not get orbiter");
                return;
            };

            //Calculate the desired position for the object - pct of TAU
            //s= 30 t = 60 p = (t/s % Tau)
            let offset = idx as f32 * TAU / child_orbiters.len() as f32;

            let point_on_circle =
                ((time.elapsed_seconds_wrapped() / orbit_controller.speed) + offset) % TAU;

            let target_x = point_on_circle.sin() * orbit_controller.radius;
            let target_z = point_on_circle.cos() * orbit_controller.radius;

            tsf.translation = tsf.translation.lerp(
                Vec3::new(target_x, 0.0, target_z) + orbit_controller.point,
                2.5 * time.delta_seconds(),
            );
        }
    }
}

mod ease_function_serde {
    use interpolation::EaseFunction;
    use serde::{
        de::{Error, Unexpected},
        Deserialize, Deserializer, Serializer,
    };

    pub fn serialize<S: Serializer>(
        &function: &EaseFunction,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        serializer.serialize_u8(function as u8)
    }

    pub fn deserialize<'a, D: Deserializer<'a>>(deserializer: D) -> Result<EaseFunction, D::Error> {
        use EaseFunction::*;

        Ok(match u8::deserialize(deserializer)? {
            0 => QuadraticIn,
            1 => QuadraticOut,
            2 => QuadraticInOut,

            3 => CubicIn,
            4 => CubicOut,
            5 => CubicInOut,

            6 => QuarticIn,
            7 => QuarticOut,
            8 => QuarticInOut,

            9 => QuinticIn,
            10 => QuinticOut,
            11 => QuinticInOut,

            12 => SineIn,
            13 => SineOut,
            14 => SineInOut,

            15 => CircularIn,
            16 => CircularOut,
            17 => CircularInOut,

            18 => ExponentialIn,
            19 => ExponentialOut,
            20 => ExponentialInOut,

            21 => ElasticIn,
            22 => ElasticOut,
            23 => ElasticInOut,

            24 => BackIn,
            25 => BackOut,
            26 => BackInOut,

            27 => BounceIn,
            28 => BounceOut,
            29 => BounceInOut,

            val @ 30.. => {
                return Err(Error::invalid_value(
                    Unexpected::Unsigned(val as u64),
                    &"an integer in `0..=29`",
                ))
            }
        })
    }
}

/// Used to wrap EaseFunction in Reflect
#[derive(Serialize, Deserialize, Default, Clone, Copy)]
pub enum EAEaseFunction {
    // Note: very few of the interpolation crate's EaseFunction "work" with a f32 > 1., they would clamp to 1.
    // (which makes sense for interpolation, not for potentially infinite difficulty)
    FromInterpolationCrate(#[serde(with = "ease_function_serde")] EaseFunction),
    #[default]
    Linear,
    LinearThenPolynomial,
    MinThenPolynomial,
}

impl EAEaseFunction {
    pub fn ease(self, f: f32) -> f32 {
        match self {
            Self::FromInterpolationCrate(ef) => f.calc(ef),
            Self::Linear => f,
            Self::LinearThenPolynomial => {
                if f <= 1.0 {
                    f
                } else {
                    f.powf(3.4)
                }
            }
            Self::MinThenPolynomial => {
                if f <= 1.0 {
                    0.
                } else {
                    f.powf(2.) - 1.
                }
            }
        }
    }
}

pub trait LerpRange<T> {
    fn lerp(&self, val: f32) -> T;
}

impl<T: AsPrimitive<f32> + LossyFrom<f32>> LerpRange<T> for Range<T> {
    fn lerp(&self, val: f32) -> T {
        T::lossy_from(self.start.as_() + ((self.end.as_() - self.start.as_()) * val))
    }
}

pub trait LossyFrom<F> {
    fn lossy_from(v: F) -> Self;
}

impl LossyFrom<f32> for u32 {
    #[inline]
    fn lossy_from(v: f32) -> u32 {
        v as u32
    }
}

impl LossyFrom<f32> for u8 {
    #[inline]
    fn lossy_from(v: f32) -> u8 {
        v as u8
    }
}

impl LossyFrom<f32> for f32 {
    #[inline]
    fn lossy_from(v: f32) -> Self {
        v
    }
}

#[derive(Component, Default, Reflect)]
#[reflect(Component)]
pub struct VisibleInStates<S: States + Default> {
    pub states: HashSet<S>,
    pub invisible_in_states: bool,
}

impl<S: States + Default> VisibleInStates<S> {
    pub fn from_state(state: S) -> Self {
        let mut states = HashSet::new();
        states.insert(state);
        Self {
            states,
            invisible_in_states: false,
        }
    }

    #[allow(dead_code)]
    pub fn invert(mut self) -> Self {
        self.invisible_in_states = !self.invisible_in_states;
        self
    }
}

pub fn visibility_on_state<S: States + Default>(
    current: Res<State<S>>,
    mut visible_in_states_query: Query<(&mut Visibility, &VisibleInStates<S>)>,
) {
    let current = current.get();
    for (mut visibility, visible_in_states) in visible_in_states_query.iter_mut() {
        if visible_in_states.states.contains(current) && !visible_in_states.invisible_in_states {
            visibility.set_if_neq(Visibility::Visible);
        } else {
            visibility.set_if_neq(Visibility::Hidden);
        }
    }
}

#[derive(Component, Default, Reflect)]
#[reflect(Component)]
pub struct SwitchImageInStates<S: States + Default> {
    pub states: HashSet<S>,
    pub image_in_states: Handle<Image>,
    pub image_out_states: Handle<Image>,
}

impl<S: States + Default> SwitchImageInStates<S> {
    #[allow(dead_code)]
    pub fn from_state(
        state: S,
        image_in_state: Handle<Image>,
        image_out_state: Handle<Image>,
    ) -> Self {
        let mut states = HashSet::new();
        states.insert(state);
        Self {
            states,
            image_in_states: image_in_state,
            image_out_states: image_out_state,
        }
    }
}

pub fn switch_image_on_state<S: States + Default>(
    current: Res<State<S>>,
    mut switch_image_in_states_query: Query<(&mut Handle<Image>, &SwitchImageInStates<S>)>,
    mut switch_ui_image_in_states_query: Query<(&mut UiImage, &SwitchImageInStates<S>)>,
) {
    let current = current.get();
    for (mut image, switch_image_in_states) in switch_image_in_states_query.iter_mut() {
        if switch_image_in_states.states.contains(current) {
            *image = switch_image_in_states.image_in_states.clone();
        } else {
            *image = switch_image_in_states.image_out_states.clone();
        }
    }
    for (mut image, switch_image_in_states) in switch_ui_image_in_states_query.iter_mut() {
        if switch_image_in_states.states.contains(current) {
            *image = switch_image_in_states.image_in_states.clone().into();
        } else {
            *image = switch_image_in_states.image_out_states.clone().into();
        }
    }
}

#[derive(Default)]
pub struct StateWatchPlugin<S: States + Default> {
    _p: PhantomData<S>,
}

impl<S: FreelyMutableState + Default> Plugin for StateWatchPlugin<S> {
    fn build(&self, app: &mut App) {
        app.init_state::<S>();
        app.add_systems(
            Update,
            (visibility_on_state::<S>, switch_image_on_state::<S>).run_if(state_changed::<S>),
        );
    }
}

fn permute_3(x: Vec3) -> Vec3 {
    (((x * 34.) + 1.) * x) % Vec3::splat(289.)
}

// MIT License. © Ian McEwan, Stefan Gustavson, Munrocket, Johan Helsing
// From https://github.com/johanhelsing/noisy_bevy
/// Simplex noise in two dimensions
pub fn simplex_noise_2d(v: Vec2) -> f32 {
    const C: Vec4 = Vec4::new(
        0.211_324_87,  // (3.0 - sqrt(3.0)) / 6.0
        0.366_025_42,  // 0.5 * (sqrt(3.0) - 1.0)
        -0.577_350_26, // -1.0 + 2.0 * C.x
        1. / 41.,
    );
    let mut i: Vec2 = (v + Vec2::dot(v, C.yy())).floor();
    let x0 = v - i + Vec2::dot(i, C.xx());
    let i1: Vec2 = if x0.x > x0.y {
        Vec2::new(1., 0.)
    } else {
        Vec2::new(0., 1.)
    };
    let x12: Vec4 = x0.xyxy() + C.xxzz() - Vec4::new(i1.x, i1.y, 0., 0.);
    i %= Vec2::splat(289.);
    let p = permute_3(permute_3(i.y + Vec3::new(0., i1.y, 1.)) + i.x + Vec3::new(0., i1.x, 1.));
    let mut m = Vec3::max(
        0.5 - Vec3::new(
            Vec2::dot(x0, x0),
            Vec2::dot(x12.xy(), x12.xy()),
            Vec2::dot(x12.zw(), x12.zw()),
        ),
        Vec3::splat(0.),
    );
    m *= m;
    m *= m;
    let x = 2. * (p * C.www()).fract() - 1.;
    let h = x.abs() - 0.5;
    let ox = (x + 0.5).floor();
    let a0 = x - ox;
    m *= 1.792_842_9 - 0.853_734_73 * (a0 * a0 + h * h);
    let g = Vec3::new(
        a0.x * x0.x + h.x * x0.y,
        a0.y * x12.x + h.y * x12.y,
        a0.z * x12.z + h.z * x12.w,
    );
    130. * Vec3::dot(m, g)
}

#[derive(Resource, Deref)]
pub struct BillboardMesh(Handle<Mesh>);

impl FromWorld for BillboardMesh {
    fn from_world(world: &mut World) -> Self {
        Self(
            world
                .resource_mut::<Assets<Mesh>>()
                .add(Rectangle::from_size(Vec2::splat(3.))),
        )
    }
}

#[derive(Component, Default)]
pub struct Billboard {
    pub angle: f32,
    pub angular_vel: f32,
}

#[derive(Component)]
pub struct BillboardCamera;

fn billboard(
    mut billboards_query: Query<(&mut Billboard, &Parent, &mut Transform)>,
    cameras_query: Query<&GlobalTransform, With<BillboardCamera>>,
    gtsfs_query: Query<&GlobalTransform>,
    time: Res<Time>,
) {
    let Ok(cam_tf) = cameras_query.get_single() else {
        return;
    };

    for (mut billboard, parent, mut tsf) in &mut billboards_query {
        billboard.angle = (billboard.angle + billboard.angular_vel * time.delta_seconds()) % TAU;

        let Ok(parent_gtsf) = gtsfs_query.get(**parent) else {
            warn!("billboard's parent missing `GlobalTransform`");
            continue;
        };

        let dir = cam_tf.translation() - parent_gtsf.translation();

        tsf.look_to(dir, Vec3::Y);
        tsf.rotate_local_y(PI);
        tsf.rotate_local_z(billboard.angle);
    }
}

/// Scales the color without scaling the alpha. For HDR.
pub trait ColorScaleExt {
    fn scale(self, scale: f32) -> Self;
}

impl ColorScaleExt for Srgba {
    fn scale(self, scale: f32) -> Self {
        let Self {
            red,
            green,
            blue,
            alpha,
        } = self;

        Self {
            red: red * scale,
            green: green * scale,
            blue: blue * scale,
            alpha,
        }
    }
}

/// Just used for debugging. Despawning counts as removal.
#[allow(dead_code)]
pub struct PanicWhenRemoved;

impl Component for PanicWhenRemoved {
    const STORAGE_TYPE: StorageType = StorageType::Table;

    fn register_component_hooks(hooks: &mut ComponentHooks) {
        hooks.on_remove(|_, _, _| {
            panic!("Removed a `PanicWhenRemoved`");
        });
    }
}

#[derive(Clone)]
pub struct AttachBundle<T: Bundle + Send + Sync> {
    bundle: T,
}

impl<T: Bundle + Send + Sync> AttachBundle<T> {
    pub fn new(bundle: T) -> Self {
        Self { bundle }
    }
}

impl<T: Bundle + Send + Sync> EntityCommand for AttachBundle<T> {
    fn apply(self, id: Entity, world: &mut World) {
        let Some(mut ent) = world.get_entity_mut(id) else {
            warn!("Coudn't get entity for AttachBundle Command");
            return;
        };

        ent.insert(self.bundle);
    }
}

pub trait AttachComponentCmdExt {
    ///Allowing Dead code because this is invoked by bevy_fabulous
    #[allow(dead_code)]
    fn attach_component<T: Bundle>(&mut self, cmd: impl Into<AttachBundle<T>>);
}

impl<'w> AttachComponentCmdExt for EntityCommands<'w> {
    fn attach_component<T: Bundle>(&mut self, cmd: impl Into<AttachBundle<T>>) {
        self.add(cmd.into());
    }
}

//Despawn self(recursive)
#[derive(Copy, Clone)]
pub enum Despawn {
    Entity,
    Recursive,
    Children,
}

impl EntityCommand for Despawn {
    fn apply(self, id: Entity, world: &mut World) {
        let Some(mut ent) = world.get_entity_mut(id) else {
            warn!("Could not get entity to despawn for Despawn EntityCommand");
            return;
        };

        match self {
            Despawn::Entity => ent.despawn(),
            Despawn::Recursive => ent.despawn_recursive(),
            Despawn::Children => {
                ent.despawn_descendants();
            }
        };
    }
}

/// Marker Component that indicates a firepoint + offset
#[derive(Component, Default, Clone)]
pub struct FirePoint(pub Vec3);
