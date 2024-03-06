use crate::player::{Health, Size, SpawnLiving};
use bevy::prelude::*;

pub struct UIPlugin;
impl Plugin for UIPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, (attach, update_hp));
    }
}

fn attach(mut cmd: Commands, mut spawn: EventReader<SpawnLiving>, hp: Query<&Health>) {
    for e in spawn.read() {
        let Ok(hp) = hp.get(e.0) else { return };
        cmd.spawn(Text2dBundle {
            text: Text::from_section(format!("{}/{}", hp.health, hp.max), TextStyle::default()),
            ..default()
        })
        .set_parent(e.0);
    }
}

fn update_hp(q_obj: Query<(&Size, &Health)>, mut q_ui: Query<(&Parent, &mut Text)>) {
    for (parent, mut text) in q_ui.iter_mut() {
        let (_, hp) = q_obj.get(parent.get()).unwrap();
        *text = Text::from_section(format!("{}/{}", hp.health, hp.max), TextStyle::default());
    }
}
