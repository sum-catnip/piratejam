#import bevy_render::globals::Globals;
@group(0) @binding(1) var<uniform> globals: Globals;
#import bevy_sprite::mesh2d_bindings::mesh
#import bevy_sprite::mesh2d_functions::{get_model_matrix, mesh2d_position_local_to_clip, mesh2d_position_local_to_world, mesh2d_position_world_to_clip}

struct Map {
    /// Size of the map, in tiles.
    /// Will be derived from underlying map texture.
    map_size: vec2<u32>,

    /// Size of the tile atlas, in pixels.
    /// Will be derived from the tile atlas texture.
    atlas_size: vec2<f32>,

    /// Size of each tile, in pixels.
    tile_size: vec2<f32>,

    /// Padding between tiles in atlas.
    inner_padding: vec2<f32>,

    /// Padding at atlas top/left and bottom/right
    outer_padding_topleft: vec2<f32>,
    outer_padding_bottomright: vec2<f32>,

    /// Relative anchor point position in a tile (in [0..1]^2)
    tile_anchor_point: vec2<f32>,

    /// fractional 2d map index -> relative local world pos
    projection: mat3x3<f32>,

    /// Global transform of the entity holding the map as transformation matrix & offset.
    /// This is currently redundant with mesh.model,
    /// which should represent the same info as a 4x4 affine matrix, but we consider it a bit
    /// more consistent in conjunction with the inverse below. May be removed in the future.
    global_transform_matrix: mat3x3<f32>,
    global_transform_translation: vec3<f32>,

    max_overhang_levels: u32,

    // -----
    /// [derived] Size of the map in world units necessary to display
    /// all tiles according to projection.
    world_size: vec2<f32>,

    /// [derived] Offset of the projected map in world coordinates
    world_offset: vec2<f32>,

    /// [derived]
    n_tiles: vec2<u32>,

    /// [derived] local world pos -> fractional 2d map index
    inverse_projection: mat2x2<f32>,

    /// [derived] Iverse of global transform of the entity holding the map as transformation matrix & offset.
    global_inverse_transform_matrix: mat3x3<f32>,
    global_inverse_transform_translation: vec3<f32>,
};

@group(2) @binding(0)
var<uniform> map: Map;

@group(2) @binding(100)
var<storage> map_texture: array<u32>;

@group(2) @binding(101)
var atlas_texture: texture_2d<f32>;

@group(2) @binding(102)
var atlas_sampler: sampler;


struct Vertex {
    @builtin(instance_index) instance_index: u32,
    @location(0) position: vec3<f32>,
    @location(1) mix_color: vec4<f32>,
};

struct VertexOutput {
    // this is `clip position` when the struct is used as a vertex stage output
    // and `frag coord` when used as a fragment stage input
    @builtin(position) position: vec4<f32>,
    @location(0) world_position: vec4<f32>,
    @location(1) mix_color: vec4<f32>,
}

/// Custom vertex shader for passing along the UV coordinate
@vertex
fn vertex(v: Vertex) -> VertexOutput {
    var out: VertexOutput;

    var model: mat4x4<f32> = get_model_matrix(v.instance_index);

    out.position = mesh2d_position_local_to_clip(model, vec4<f32>(v.position, 1.0));
    out.world_position = mesh2d_position_local_to_world(model, vec4<f32>(v.position, 1.0));
    out.mix_color = v.mix_color;

    var world_position = out.world_position.xy;
    var pos = world_to_tile_and_offset(world_position);
    var index = get_tile_index(pos.tile);

    //out.world_position.y = sin(globals.time + f32(pos.tile.x) + f32(pos.tile.y));
    //out.position = mesh2d_position_world_to_clip(out.world_position);
    //if pos.tile.x % 2 == 0 {
    //    //out.world_position.x *= sin(globals.time);
    //    //out.world_position.y *= sin(globals.time);
    //    out.world_position.y *= sin(globals.time);
    //    out.position = mesh2d_position_world_to_clip(out.world_position);
    //    out.mix_color = vec4<f32>(f32(pos.tile.x), f32(pos.tile.y), 0.0, 1.0);
    //}
    //out.mix_color = vec4<f32>(f32(pos.tile.x), f32(pos.tile.y), 0.0, 1.0);

    return out;
}

/// Map position incl fractional part for this position.
fn world_to_map(map: Map, world_position: vec2<f32>) -> vec2<f32> {
    // Steps:
    // - Apply inverse global transform
    // - Adjust for `map.world_offset` (where in the mesh tile 0,0 should be)
    // - Scale according to `map.tile_size`
    // - Apply inverse map projection for tile distortion (eg iso)
    var local_world_pos = map.global_inverse_transform_matrix * vec3<f32>(world_position, 0.0) + map.global_inverse_transform_translation;
    var pos = (local_world_pos.xy - map.world_offset) / map.tile_size;
    return map.inverse_projection * pos;
}

fn map_to_world(map: Map, map_position: vec2<f32>) -> vec3<f32> {
    // Steps:
    // - Apply map projection (to compensate for eg iso view)
    // - scale according to `map.tile_size`
    // - Adjust for `map.world_offset` (where in the mesh tile 0,0 should be)
    // - Apply global transform
    return map.global_transform_matrix * ((map.projection * vec3<f32>(map_position, 0.0)) * vec3<f32>(map.tile_size, 1.0) + vec3<f32>(map.world_offset, 0.0)) + map.global_transform_translation;
}

/// Position (world/pixel units) in tilemap atlas of the top left corner
/// of the tile with the given index
fn atlas_index_to_position(map: Map, index: u32) -> vec2<f32> {
    var index_f = f32(index);
    var index_y = floor(index_f / f32(map.n_tiles.x));
    var index_x = index_f - index_y * f32(map.n_tiles.x);
    var index2d = vec2<f32>(index_x, index_y);

    var pos = index2d * (map.tile_size + map.inner_padding) + map.outer_padding_topleft;
    return pos;
}

/// Compute offset into the tile by world position and world position of tile reference point
fn world_to_tile_offset(world_position: vec2<f32>, world_tile_base: vec2<f32>) -> vec2<f32> {
    return vec2<f32>(1.0, -1.0) * (world_position - world_tile_base);
}

/// Sample tile from the tile atlas
/// tile_index: Index of the tile in the atlas
/// tile_offset: Offset from tile anchor point in pixel/world coordinates
fn sample_tile(
    map: Map,
    tile_index: u32,
    tile_offset: vec2<f32>,
) -> vec4<f32> {

    //  +-----------+    :    +------------+
    //  |           |    :    |            |
    //  a           |    :    |            |
    //  |           |    :    |            |
    //  +-----------+    :    +------------+
    //                   :
    //  .................:..................
    //                   :
    //  +-----------+    :    +------------+
    //  |           |    :    |            |
    //  |           |    :    |            |
    //  |           |    :    |            |
    //  +-----------+    :    +------------+

    //  [   TILE    ] Padding [    TILE    ]

    var tile_start = atlas_index_to_position(map, tile_index);
    var rect_offset = tile_offset + map.tile_anchor_point * map.tile_size;
    var total_offset = tile_start + rect_offset;

    // At most half of the inner "padding" is still rendered
    // as overhang of any given tile.
    // Outer padding is not taken into account
    var max_overhang = map.inner_padding / 2.0;

    // Outside of "our" part of the padding, dont render anything as part of this tile,
    // as it might be used for overhang of a neighbouring tile in the tilemap
    if rect_offset.x < -max_overhang.x || rect_offset.y < -max_overhang.y || rect_offset.x > (map.tile_size.x + max_overhang.x) || rect_offset.y > (map.tile_size.y + max_overhang.y) {
        return vec4<f32>(0.0, 0.0, 0.0, 0.0);
    }
    //var color = textureSample(
    //    atlas_texture, atlas_sampler, total_offset / map.atlas_size
    //);
    // work around uniformity constraints, but doesnt seem to actually speed things up
    var color = sample(atlas_texture, atlas_sampler, total_offset / map.atlas_size);

    return color;
}

fn sample(atlas_texture: texture_2d<f32>, atlas_sampler: sampler, uv: vec2<f32>) -> vec4<f32> {
    return textureSample(atlas_texture, atlas_sampler, uv);
}

/// 2d map tile position and offset in map tile coordinates
struct MapPosition {
    /// The 2d tile position on the map
    tile: vec2<i32>,
    /// Offset in pixels/world coordinates from the reference position of that tile
    offset: vec2<f32>
};


/// Figure out where in the map (tile position & offset) this world position is.
fn world_to_tile_and_offset(
    world_position: vec2<f32>
) -> MapPosition {
    var out: MapPosition;

    // Map position including fractional part
    var pos = world_to_map(map, world_position);

    // Integer part of map position (tile coordinate)
    var tile = floor(pos);
    out.tile = vec2<i32>(tile);

    // World position of tile reference point
    var world_tile_base = map_to_world(map, tile).xy;
    out.offset = world_to_tile_offset(world_position, world_tile_base);

    return out;
}

///
fn get_tile_index(map_position: vec2<i32>) -> u32 {
    //return u32(textureLoad(map_texture, map_position).r);
    return map_texture[map_position.y * i32(map.map_size.x) + map_position.x];
}

fn blend(c0: vec4<f32>, c1: vec4<f32>) -> vec4<f32> {
    return mix(c0, c1, c1.a);
}

fn is_valid_tile(map: Map, tile: vec2<i32>) -> bool {
    if tile.x < 0 || tile.y < 0 {
        return false;
    }
    let map_size = vec2<i32>(map.map_size);
    if tile.x >= map_size.x || tile.y >= map_size.y {
        return false;
    }
    return true;
}

///
///
/// tile_index: Tile index in the atlas
/// pos: The original map position
/// tile_offset: The offset of the tile (in number of whole tiles) to sample from
fn sample_neighbor_tile_index(tile_index: u32, pos: MapPosition, tile_offset: vec2<i32>) -> vec4<f32> {
    // Position in the neighboring tile (in world coordinates),
    // that matches 'pos' in the original tile

    // TODO: Consider precomputing this before shader instantiation for the 8 possible offsets.
    var overhang = (map.projection * vec3<f32>(vec2<f32>(-tile_offset), 0.0)).xy * map.tile_size;

    var offset = pos.offset + vec2<f32>(1.0, -1.0) * overhang;
    return sample_tile(map, tile_index, offset);
}

/// pos: The map position to sample
/// tile_offset: The offset of the tile (in number of whole tiles) to sample from
fn sample_neighbor(pos: MapPosition, tile_offset: vec2<i32>) -> vec4<f32> {
    // integral position of the neighbouring tile
    var tile = pos.tile + tile_offset;
    if !is_valid_tile(map, tile) {
        return vec4<f32>(0.0, 0.0, 0.0, 0.0);
    }

    // kind of tile being displayed at that position
    var tile_index = get_tile_index(tile);
    return sample_neighbor_tile_index(tile_index, pos, tile_offset);
}

fn sample_neighbor_if_ge(index: u32, pos: MapPosition, tile_offset: vec2<i32>) -> vec4<f32> {
    // integral position of the neighbouring tile
    var tile = pos.tile + tile_offset;
    if !is_valid_tile(map, tile) {
        return vec4<f32>(0.0, 0.0, 0.0, 0.0);
    }

    // kind of tile being displayed at that position
    var tile_index = get_tile_index(tile);
    if tile_index >= index {
        return sample_neighbor_tile_index(tile_index, pos, tile_offset);
    }

    return vec4<f32>(0.0, 0.0, 0.0, 0.0);
}

fn render_dominance_overhangs(color: vec4<f32>, index: u32, pos: MapPosition) -> vec4<f32> {
    var max_index = min(map.n_tiles.x * map.n_tiles.y, index + map.max_overhang_levels);
    var c = color;

    // Note: For some reason on OSX, the use of for loops fails silently (produces pure red output
    // in our case), while a loop { ... } seems to work just fine.
    var idx = index + u32(1);
    loop {
        if idx >= max_index { break; }

        // first render all the diagonal overhangs
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(-1, -1)));
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(-1, 1)));
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(1, -1)));
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(1, 1)));

        // Now all the orthogonal ones
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(-1, 0)));
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(1, 0)));
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(0, -1)));
        c = blend(c, sample_neighbor_if_ge(idx, pos, vec2<i32>(0, 1)));

        idx++;
    }

    return c;
}

/// Render underhangs for perspective projection
/// color: The color of the current fragment so far
/// pos: The position of the current fragment as map position
fn render_perspective_underhangs(color: vec4<f32>, pos: MapPosition) -> vec4<f32> {
    var c = color;

    // Form is PERSPECTIVE_UNDER_{X}{Y}
    // whereas {X} and {Y} are replaced with one of:
    // N: Negative (-1)
    // P: Positive (1)
    // Z: Zero (0)
    #ifdef PERSPECTIVE_UNDER_NN
    c = blend(c, sample_neighbor(pos, vec2<i32>(-1, -1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_NP
    c = blend(c, sample_neighbor(pos, vec2<i32>(-1, 1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_PN
    c = blend(c, sample_neighbor(pos, vec2<i32>(1, -1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_PP
    c = blend(c, sample_neighbor(pos, vec2<i32>(1, 1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_ZN
    c = blend(c, sample_neighbor(pos, vec2<i32>(0, -1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_NZ
    c = blend(c, sample_neighbor(pos, vec2<i32>(-1, 0)));
    #endif

    #ifdef PERSPECTIVE_UNDER_ZP
    c = blend(c, sample_neighbor(pos, vec2<i32>(0, 1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_PZ
    c = blend(c, sample_neighbor(pos, vec2<i32>(1, 0)));
    #endif

    return c;
}


fn render_perspective_overhangs(color: vec4<f32>, pos: MapPosition) -> vec4<f32> {
    var c = color;

    #ifdef PERSPECTIVE_UNDER_ZN
    c = blend(c, sample_neighbor(pos, vec2<i32>(0, 1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_NZ
    c = blend(c, sample_neighbor(pos, vec2<i32>(1, 0)));
    #endif

    #ifdef PERSPECTIVE_UNDER_ZP
    c = blend(c, sample_neighbor(pos, vec2<i32>(0, -1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_PZ
    c = blend(c, sample_neighbor(pos, vec2<i32>(-1, 0)));
    #endif

    #ifdef PERSPECTIVE_UNDER_NN
    c = blend(c, sample_neighbor(pos, vec2<i32>(1, 1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_NP
    c = blend(c, sample_neighbor(pos, vec2<i32>(1, -1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_PN
    c = blend(c, sample_neighbor(pos, vec2<i32>(-1, 1)));
    #endif

    #ifdef PERSPECTIVE_UNDER_PP
    c = blend(c, sample_neighbor(pos, vec2<i32>(-1, -1)));
    #endif

    return c;
}

fn desaturate(color: vec4<f32>, amount: f32) -> vec4<f32> {
    var luminance = vec4<f32>(0.299, 0.587, 0.114, 0.0);
    var gr = dot(luminance, color);
    var gray = vec4<f32>(gr, gr, gr, color.a);
    var amnt = vec4<f32>(amount, amount, amount, amount);
    return mix(color, gray, amnt);
}

fn mod289v3f(x: vec3f)     -> vec3f { return x - floor(x / 289.0) * 289.0; }
fn mod289v2f(x: vec2f)     -> vec2f { return x - floor(x / 289.0) * 289.0; }
fn mod7v3f(x: vec3f)       -> vec3f { return x - floor(x / 6.999999) * 6.999999; }
fn permute289v3f(x: vec3f) -> vec3f { return mod289v3f((34.0 * x + 10.0) * x); }

fn cellular2D(P: vec2f) -> vec2f {
    let K = 0.142857142857;
    let Ko = 0.428571428571;
    let jitter = 1.0;
    let Pi = mod289v2f(floor(P));
    let Pf = fract(P);
    let Oi = vec3f(-1.0, 0.0, 1.0);
    let Of = vec3f(-0.5, 0.5, 1.5);
    let px = permute289v3f(Pi.x + Oi);
    var p  = permute289v3f(px.x + Pi.y + Oi);
    var ox = vec3f(fract(p*K) - Ko);
    var oy = mod7v3f(floor(p*K))*K - Ko;
    var dx = vec3f(Pf.x + 0.5 + jitter*ox);
    var dy = vec3f(Pf.y - Of + jitter*oy);
    var d1 = vec3f(dx * dx + dy * dy);
    p = permute289v3f(px.y + Pi.y + Oi);
    ox = fract(p*K) - Ko;
    oy = mod7v3f(floor(p*K))*K - Ko;
    dx = Pf.x - 0.5 + jitter*ox;
    dy = Pf.y - Of + jitter*oy;
    var d2 = vec3f(dx * dx + dy * dy);
    p = permute289v3f(px.z + Pi.y + Oi);
    ox = fract(p*K) - Ko;
    oy = mod7v3f(floor(p*K))*K - Ko;
    dx = Pf.x - 1.5 + jitter*ox;
    dy = Pf.y - Of + jitter*oy;
    let d3 = vec3f(dx * dx + dy * dy);
    let d1a = min(d1, d2);
    d2 = max(d1, d2);
    d2 = min(d2, d3);
    d1 = min(d1a, d2);
    d2 = max(d1a, d2);
    d1 = select(d1.yxz, d1, (d1.x < d1.y));
    d1 = select(d1.zyx, d1, (d1.x < d1.z));
    d1 = vec3f( d1.x, min(d1.yz, d2.yz) );
    d1 = vec3f( d1.x, min(d1.y, d1.z), d1.z );
    d1 = vec3f( d1.x, min(d1.y, d2.x), d1.z );
    return sqrt(d1.xy);
}

fn random2(p: vec2f) -> vec2f {
    return fract(sin(vec2f(dot(p, vec2f(127.1, 331.7)), dot(p, vec2f(269.5, 183.3))))*43758.5453);
}

fn voronoi(p: vec2f) -> vec3f {
    let n = floor(p);
    let f = fract(p);

    // first pass: regular voronoi
    var mg: vec2f;
    var mr: vec2f;
    var md = 8.;
    for (var j = -1; j <= 1; j++) {
        for (var i = -1; i <= 1; i++) {
            let g = vec2f(f32(i), f32(j));
            var o = random2(n + g);
            o = 0.5 + 0.5 * sin(globals.time + 6.2832 * o);

            let r = g + o - f;
            let d = dot(r, r);
            if (d < md) {
                md = d;
                mr = r;
                mg = g;
            }
        }
    }

    // second pass: distance to borders
    md = 8.;
    for (var j = -2; j <= 2; j++) {
        for (var i = -2; i <= 2; i++) {
            let g = mg + vec2f(f32(i), f32(j));
            var o = random2(n + g);
            o = 0.5 + 0.5 * sin(globals.time + 6.2832 * o);

            let r = g + o - f;
            if (dot(mr - r, mr - r) > 0.00001) {
                md = min(md, dot(0.5 * (mr + r), normalize(r - mr)));
            }
        }
    }

    return vec3f(md, mr);
}

@fragment
fn fragment(
    in: VertexOutput
) -> @location(0) vec4<f32> {
    var world_position = in.world_position.xy;
    var color = vec4<f32>(0.0, 0.0, 0.0, 0.0);
    var pos = world_to_tile_and_offset(world_position);
    var index = get_tile_index(pos.tile);

    var sample_color = sample_tile(map, index, pos.offset);

    //#ifdef PERSPECTIVE_UNDERHANGS
    //if sample_color.a < 1.0 {
    //    color = render_perspective_underhangs(color, pos);
    //}
    //#endif // PERSPECTIVE_UNDERHANGS

    if is_valid_tile(map, pos.tile) {
        color = blend(color, sample_color);
        //var F = cellular2D(world_position / 100.);
        //var facets = 0.1 + (F.y - F.x);
        ////var dots = smoothstep(0.05, 0.1, F.x);
        //var n = facets;
        //color = vec4f(n, n, n, 1.);

    }

    let c = voronoi(world_position / 20.);
    var color3 = vec3f(0.);
    // isolines
    color3 = (1. - c.x) * vec3f(1.);

    // borders
    color3 = mix(vec3f(1.), color3, smoothstep(0.01, 0.02, c.x));
    // points
    //let dd = length(c.yz);
    //color3 += vec3f(1.) * (1. - smoothstep(0., 0.04, dd));
    color *= vec4(color3, 1.);
    //color.r = 0.;
    //color.g = 0.;
    //color.b = 0.;
    //color.a *= 1.;

    //#ifdef DOMINANCE_OVERHANGS
    //color = render_dominance_overhangs(color, index, pos);
    //#endif

    //#ifdef PERSPECTIVE_OVERHANGS
    //color = render_perspective_overhangs(color, pos);
    //#endif

    return color;
}
