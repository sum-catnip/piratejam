// Bevy code commonly triggers these lints and they may be important signals
// about code quality. They are sometimes hard to avoid though, and the CI
// workflow treats them as errors, so this allows them throughout the project.
// Feel free to delete this line.
#![allow(clippy::too_many_arguments, clippy::type_complexity)]
mod worldgen;
use worldgen::WorldGenPlugin;

mod player;
use player::PlayerPlugin;

mod gamepad;
use gamepad::GamepadPlugin;

use bevy::prelude::*;

fn main() {
    App::new()
        .add_plugins(
            DefaultPlugins
                .set(ImagePlugin::default_nearest())
        )
        .insert_resource(Msaa::Off)
        .add_plugins((DefaultPlugins, WorldGenPlugin, PlayerPlugin, GamepadPlugin))
        .add_systems(Startup, setup)
        .add_systems(Update, camera)
        .run();
}

fn camera(
    time: Res<Time>,
    kb: Res<ButtonInput<KeyCode>>,
    mut q: Query<(&mut Transform, &mut OrthographicProjection), With<Camera>>,
) {
    let (mut transform, mut ortho) = q.single_mut();
    let mut dir = Vec3::ZERO;

    if kb.pressed(KeyCode::KeyA) {
        dir -= Vec3::new(1.0, 0.0, 0.0);
    }

    if kb.pressed(KeyCode::KeyD) {
        dir += Vec3::new(1.0, 0.0, 0.0);
    }

    if kb.pressed(KeyCode::KeyW) {
        dir += Vec3::new(0.0, 1.0, 0.0);
    }

    if kb.pressed(KeyCode::KeyS) {
        dir -= Vec3::new(0.0, 1.0, 0.0);
    }

    if kb.pressed(KeyCode::KeyZ) {
        ortho.scale += 0.1;
    }

    if kb.pressed(KeyCode::KeyX) {
        ortho.scale -= 0.1;
    }

    if ortho.scale < 0.5 {
        ortho.scale = 0.5;
    }

    let z = transform.translation.z;
    transform.translation += time.delta_seconds() * dir * 500. * (ortho.scale + 1.);
    // Important! We need to restore the Z values when moving the camera around.
    // Bevy has a specific camera setup and this can mess with how our layers are shown.
    transform.translation.z = z;
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
}
