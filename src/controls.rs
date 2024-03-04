use bevy::prelude::*;

#[derive(Debug)]
pub enum ShootDirection { Left, Right }

#[derive(Event)]
pub struct PlayerShot(pub ShootDirection);

pub struct ControlsPlugin;

impl Plugin for ControlsPlugin {
    fn build(&self, app: &mut App) {
        app.add_event::<PlayerShot>();
    }
}
