use bevy::{ecs::system::EntityCommand, prelude::*};
use rand::{thread_rng, Rng};

pub fn transform_utils_plugin(app: &mut App) {
    app.add_systems(Update, (rotate_over_time, bob, scale, jitter_scale));
}

#[derive(Component, Reflect, Clone)]
#[reflect(Component)]
pub struct Rotate {
    pub rotation_rate: f32,
    pub dir: Vec3,
}

fn rotate_over_time(mut rotators: Query<(&mut Transform, &Rotate)>, time: Res<Time>) {
    for (mut tsf, rotator) in rotators.iter_mut() {
        let rotation = rotator.dir.normalize() * time.delta_seconds() * rotator.rotation_rate;
        tsf.rotate(Quat::from_euler(
            EulerRot::XYZ,
            rotation.x,
            rotation.y,
            rotation.z,
        ));
    }
}

#[derive(Component, Reflect, Clone)]
#[reflect(Component)]
pub struct Bob {
    amplitude: f32,
    frequency: f32,
    anchor: f32,
}

fn bob(mut bobbers: Query<(&mut Transform, &Bob)>, time: Res<Time>) {
    for (mut tsf, bobber) in bobbers.iter_mut() {
        tsf.translation.y =
            bobber.anchor + (time.elapsed_seconds() * bobber.frequency).sin() * (bobber.amplitude);
    }
}

#[derive(Component, Reflect, Clone)]
#[reflect(Component)]
pub struct ScaleOverTime {
    pub factor: Vec3,
    pub frequency: f32,
    pub base_scale: Vec3,
}

impl Default for ScaleOverTime {
    fn default() -> Self {
        Self {
            factor: Vec3::splat(1.5),
            frequency: 8.0,
            base_scale: Vec3::ONE,
        }
    }
}

fn scale(mut scaler: Query<(&mut Transform, &ScaleOverTime)>, time: Res<Time>) {
    for (mut tsf, scaler) in scaler.iter_mut() {
        tsf.scale = scaler.base_scale
            + (time.elapsed_seconds() * scaler.frequency).sin().abs()
                * (scaler.factor - scaler.base_scale);
    }
}

fn jitter_scale(mut scaler: Query<(&mut Transform, &JitteredScaleOverTime)>, time: Res<Time>) {
    for (mut tsf, scaler) in scaler.iter_mut() {
        tsf.scale = scaler.scale.base_scale
            + ((time.elapsed_seconds() + scaler.time_offset) * scaler.scale.frequency)
                .sin()
                .abs()
                * (scaler.scale.factor - scaler.scale.base_scale);
    }
}

//Attaches a scale over time with a random offset
#[derive(Component, Reflect, Clone)]
#[reflect(Component)]
pub struct JitteredScaleOverTime {
    pub scale: ScaleOverTime,
    pub time_offset: f32,
}

impl EntityCommand for JitteredScaleOverTime {
    fn apply(self, id: Entity, world: &mut World) {
        if let Some(mut entity) = world.get_entity_mut(id) {
            entity.insert(Self {
                time_offset: thread_rng().gen_range(0.0..10.0),
                ..self
            });
        }
    }
}

//TODO: EntityCommandsExtensionTrait
