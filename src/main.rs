use ggez::event::{self, EventHandler, MouseButton};
use ggez::graphics;
use ggez::nalgebra::{Point2, Vector2};
use ggez::{Context, ContextBuilder, GameResult};
use std::collections::HashMap;
use std::rc::Rc;

struct MyGame {
    focus_color: f32,
    focused: bool,
    build_plan: Option<TileRole>,
    selecting: Option<(Point2<f32>, Point2<f32>)>,
    pointing_at_tile: (usize, usize),
    pointing_at_unit: Option<UnitId>,
    units: Vec<Unit>,
    window_size: Vector2<f32>,
    game_map: GameMap,
    conveyor_animation: usize,
    worker_base_image: graphics::Image,
    worker_turret_image: graphics::Image,
    sand_image: graphics::Image,
    rock_images: Vec<graphics::Image>,
    conveyor_straight_images: Vec<graphics::Image>,
    conveyor_curved_images: Vec<graphics::Image>,
    conveyor_ghost_image: graphics::Image,
    mine_image: graphics::Image,
    link_image: graphics::Image,
}

struct GameMap {
    tiles: Vec<GroundTile>,
    width: usize,
    height: usize,
}

const NEIGHBOURS: [(i32, i32); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

const MAX_ELEVATION: f32 = -0.33;

impl GameMap {
    fn tile(&self, x: usize, y: usize) -> &GroundTile {
        &self.tiles[y * self.width + x]
    }

    fn tile_mut(&mut self, (x, y): (usize, usize)) -> &mut GroundTile {
        &mut self.tiles[y * self.width + x]
    }

    fn can_reach_neighbour(
        &self,
        source_x: i32,
        source_y: i32,
        x_offset: i32,
        y_offset: i32,
    ) -> bool {
        let target_x = source_x + x_offset;
        let target_y = source_y + y_offset;

        // Check bounds
        if source_x < 0
            || source_y < 0
            || source_x >= self.width as i32
            || source_y >= self.height as i32
        {
            return false;
        }
        if target_x < 0
            || target_y < 0
            || target_x >= self.width as i32
            || target_y >= self.height as i32
        {
            return false;
        }

        // Bounds are good, guaranteed to fit in a usize
        let (source_x, target_x) = (source_x as usize, target_x as usize);
        let (source_y, target_y) = (source_y as usize, target_y as usize);

        // Consider x going directly (diagonally) to y; it can only reach if
        // the two 'o' are both free.
        // xo
        // oy
        let diagonally_okay = if x_offset != 0 && y_offset != 0 {
            self.tile(source_x, target_y).passable() && self.tile(target_x, source_y).passable()
        } else {
            true
        };

        diagonally_okay && self.tile(target_x, target_y).passable()
    }

    fn path_flow_to_target(&self, target: Point2<f32>) -> HashMap<(i32, i32), Vector2<f32>> {
        let (x, y) = ((target.x / 32.0) as i32, (target.y / 32.0) as i32);

        fn cost(x_offset: i32, y_offset: i32) -> u32 {
            match x_offset.abs() + y_offset.abs() {
                2 => 14, // ~= sqrt(2) * 10, diagonal of a square
                _ => 10, // 1 * 10
            }
        }

        pathfinding::directed::dijkstra::dijkstra_all(&(x, y), |(x, y)| {
            NEIGHBOURS
                .iter()
                .filter(|(x_offset, y_offset)| {
                    self.can_reach_neighbour(*x, *y, *x_offset, *y_offset)
                })
                .map(|(x_offset, y_offset)| {
                    ((x + *x_offset, y + *y_offset), cost(*x_offset, *y_offset))
                })
                .collect::<Vec<_>>()
        })
        .into_iter()
        .map(|(position, (parent, _cost))| {
            let x_offset = position.0 - parent.0;
            let y_offset = position.1 - parent.1;

            // Can probably be const'd into a global map
            let direction = -Vector2::new(x_offset as f32, y_offset as f32).normalize();

            (position, direction)
        })
        .collect::<HashMap<_, _>>()
    }

    // TODO: Size
    fn raycast_to_target(&self, unit: &Unit, source: Point2<f32>, target: Point2<f32>) -> bool {
        let direction = (target - source).normalize();
        let step = direction * unit.speed();
        let mut position = source;
        let mut last_tile = self.to_tile(position);
        let steps = ((target - source).magnitude() / unit.speed()).floor() as usize;

        for _ in 0..steps {
            position += step;

            let new_tile = self.to_tile(position);

            if last_tile != new_tile {
                last_tile = new_tile;

                if !self.tile(new_tile.0, new_tile.1).passable() {
                    return false;
                }
            }
        }

        true
    }

    fn confine_to_map_limits(&self, x: i32, y: i32) -> (usize, usize) {
        let x = match x {
            i if i < 0 => 0,
            i if i >= self.width as i32 => self.width - 1,
            i => i as usize,
        };

        let y = match y {
            i if i < 0 => 0,
            i if i >= self.height as i32 => self.height - 1,
            i => i as usize,
        };

        (x, y)
    }

    fn tile_in_direction(&self, (x, y): (usize, usize), direction: Direction) -> (usize, usize) {
        let (x, y) = (x as i32, y as i32);
        let (x, y) = match direction {
            Direction::Up => (x, y - 1),
            Direction::Right => (x + 1, y),
            Direction::Down => (x, y + 1),
            Direction::Left => (x - 1, y),
        };

        self.confine_to_map_limits(x, y)
    }

    fn to_tile(&self, point: Point2<f32>) -> (usize, usize) {
        let (x, y) = (
            (point.x / 32.0).floor() as i32,
            (point.y / 32.0).floor() as i32,
        );

        self.confine_to_map_limits(x, y)
    }
}

#[derive(Default, Clone)]
struct GroundTile {
    /// The maximum unit size that can pass through this tile.
    /// 0 is impassable, 1 means the tile is free, 2 means this tile and all
    /// its neighbours are free, and so on.
    size: u8,
    role: TileRole,
    build_plan: Option<TileRole>,
    mining_progress: u8,
}

impl GroundTile {
    fn passable(&self) -> bool {
        match self.role {
            TileRole::Ground => true,
            TileRole::Conveyor { .. } => true,
            TileRole::Rock => false,
        }
    }
}

impl MyGame {
    fn new(ctx: &mut Context) -> GameResult<Self> {
        let worker_base_image = graphics::Image::new(ctx, "/worker_base.png")?;
        let worker_turret_image = graphics::Image::new(ctx, "/worker_turret.png")?;
        let sand_image = graphics::Image::new(ctx, "/sand.png")?;
        let mut rock_images = vec![];
        for frame in 1..=7 {
            rock_images.push(graphics::Image::new(
                ctx,
                format!("/rock_sprite_{}.png", frame),
            )?);
        }
        let mut conveyor_straight_images = vec![];
        let mut conveyor_curved_images = vec![];
        for frame in 0..=13 {
            conveyor_straight_images.push(graphics::Image::new(
                ctx,
                format!("/conveyor_straight_{:02}.png", frame),
            )?);
            conveyor_curved_images.push(graphics::Image::new(
                ctx,
                format!("/conveyor_curved_{:02}.png", frame),
            )?);
        }
        let conveyor_ghost_image = graphics::Image::new(ctx, "/conveyor_straight_ghost.png")?;
        let mine_image = graphics::Image::new(ctx, "/mine.png")?;
        let link_image = graphics::Image::new(ctx, "/link.png")?;

        use noise::{Billow, NoiseFn};
        let billow = Billow::new();

        let mut game_map = GameMap {
            width: 100,
            height: 100,
            tiles: vec![GroundTile::default(); 100 * 100],
        };

        for y in 0..game_map.height {
            for x in 0..game_map.width {
                let elevation = billow.get([x as f64 / 100.0, y as f64 / 100.0]) as f32;
                if elevation > MAX_ELEVATION {
                    game_map.tiles[y * game_map.width + x].role = TileRole::Rock;
                }
            }
        }

        Ok(Self {
            focus_color: 0.2,
            focused: true,
            build_plan: None,
            selecting: None,
            units: Vec::default(),
            window_size: Vector2::new(0.0, 0.0),
            game_map,
            conveyor_animation: 0,
            worker_base_image,
            worker_turret_image,
            sand_image,
            rock_images,
            conveyor_straight_images,
            conveyor_curved_images,
            conveyor_ghost_image,
            mine_image,
            link_image,
            pointing_at_tile: (0, 0),
            pointing_at_unit: None,
        })
    }

    fn pointing_at(&self, tile_role: TileRole) -> bool {
        self.game_map
            .tile(self.pointing_at_tile.0, self.pointing_at_tile.1)
            .role
            == tile_role
    }

    fn draw_unit(&self, ctx: &mut Context, unit: &Unit) -> GameResult<()> {
        // Draw base
        let draw_param = graphics::DrawParam::new()
            .offset(Point2::new(0.5, 0.5))
            .rotation(to_angle(unit.rotation))
            .dest(unit.position);

        let unit_image = match unit.role {
            UnitRole::Worker => &self.worker_base_image,
            UnitRole::Ore => self.rock_images.last().unwrap(),
        };
        graphics::draw(ctx, unit_image, draw_param)?;

        // Draw worker turret
        if unit.role == UnitRole::Worker {
            let rotation = if let UnitAction::Mining {
                tile,
                active: _,
                animation: _,
            } = unit.action
            {
                (tile_center(tile) - unit.position).normalize()
            } else {
                unit.rotation
            };

            let draw_param = graphics::DrawParam::new()
                .offset(Point2::new(0.5, 0.5))
                .rotation(to_angle(rotation))
                .dest(unit.position);

            graphics::draw(ctx, &self.worker_turret_image, draw_param)?;
        }

        Ok(())
    }

    fn pointing_at_unit(&self) -> Option<&Unit> {
        for unit in self.units.iter() {
            if Some(&unit.id) == self.pointing_at_unit.as_ref() {
                return Some(unit);
            }
        }

        None
    }

    fn unit_with_id(&self, id: &UnitId) -> Option<usize> {
        for (u, unit) in self.units.iter().enumerate() {
            if &unit.id == id {
                return Some(u);
            }
        }
        None
    }
}

struct Unit {
    id: UnitId,
    position: Point2<f32>,
    rotation: Vector2<f32>,
    intended_direction: Option<Vector2<f32>>,
    target: Option<Point2<f32>>,
    path: Option<Rc<HashMap<(i32, i32), Vector2<f32>>>>,
    selected: bool,
    role: UnitRole,
    action: UnitAction,
    hooking_unit: Option<UnitId>,
    being_hooked_by: Option<UnitId>,
}

impl Unit {
    fn selected_by(&self, point: Point2<f32>, size: Vector2<f32>) -> bool {
        is_between(self.position, (point, point + size))
    }

    fn speed(&self) -> f32 {
        5.0
    }
}

#[derive(Default, Debug, Eq, Clone)]
struct UnitId(Rc<()>);

impl PartialEq for UnitId {
    fn eq(&self, other: &UnitId) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(PartialEq, Eq)]
enum UnitRole {
    Worker,
    Ore,
}

#[derive(Debug)]
enum UnitAction {
    Normal,
    Mining {
        tile: (usize, usize),
        active: bool,
        animation: u8,
    },
    Building {
        tile: (usize, usize),
    },
    StartHooking {
        hook_unit: UnitId,
    },
    StopHooking,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TileRole {
    Ground,
    Rock,
    Conveyor { direction: Direction },
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn to_vector(&self) -> Vector2<f32> {
        match self {
            Direction::Up => Vector2::new(0.0, -1.0),
            Direction::Right => Vector2::new(1.0, 0.0),
            Direction::Down => Vector2::new(0.0, 1.0),
            Direction::Left => Vector2::new(-1.0, 0.0),
        }
    }

    fn next(&self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }
}

impl Default for TileRole {
    fn default() -> Self {
        TileRole::Ground
    }
}

fn to_rect(p1: Point2<f32>, p2: Point2<f32>) -> (Point2<f32>, Vector2<f32>) {
    let rect_p1 = Point2::new(p1.x.min(p2.x), p1.y.min(p2.y));
    let rect_p2 = Vector2::new((p2.x - p1.x).abs(), (p2.y - p1.y).abs());

    (rect_p1, rect_p2)
}

fn to_angle(vector: Vector2<f32>) -> f32 {
    vector.y.atan2(vector.x) + (std::f32::consts::PI / 2.0)
}

fn is_between(p: Point2<f32>, (p1, p2): (Point2<f32>, Point2<f32>)) -> bool {
    p.x >= p1.x && p.x <= p2.x && p.y >= p1.y && p.y <= p2.y
}

fn tile_center(tile: (usize, usize)) -> Point2<f32> {
    Point2::new(tile.0 as f32 * 32.0 + 16.0, tile.1 as f32 * 32.0 + 16.0)
}

fn draw_circle(
    ctx: &mut Context,
    position: Point2<f32>,
    radius: f32,
    color: graphics::Color,
    fill: bool,
) -> GameResult<()> {
    let mesh = graphics::Mesh::new_circle(
        ctx,
        if fill {
            graphics::DrawMode::fill()
        } else {
            graphics::DrawMode::stroke(1.0)
        },
        position,
        radius,
        2.0,
        color,
    )?;
    graphics::draw(ctx, &mesh, graphics::DrawParam::default())?;

    Ok(())
}

fn interpolate_flow(
    flow: &HashMap<(i32, i32), Vector2<f32>>,
    point: Point2<f32>,
) -> Option<&Vector2<f32>> {
    let current_tile = point / 32.0;
    let current_tile = Point2::new(current_tile.x.floor(), current_tile.y.floor());
    //    let position_within_tile = point - current_tile;
    //
    //    let neighbour_contribution = [f32; NEIGHBOURS.len()];
    //
    //    for (i, (x, y)) in NEIGHBOURS.iter().enumerate() {
    //
    //    }
    //    NEIGHBOURS.iter()
    //        .map(|(x_offset, y_offset)| (x_offset, y_offset, ))
    //        .map(|offset| ...);

    flow.get(&(current_tile.x as i32, current_tile.y as i32))
}

impl EventHandler for MyGame {
    fn update(&mut self, _ctx: &mut Context) -> GameResult<()> {
        if self.focused && self.focus_color < 0.3 {
            self.focus_color += 0.01;
        } else if !self.focused && self.focus_color > 0.2 {
            self.focus_color -= 0.01;
        }

        self.conveyor_animation =
            (self.conveyor_animation + 1) % (self.conveyor_straight_images.len() * 3);

        let mut create_units = vec![];

        for u in 0..self.units.len() {
            // Tow things hooked to this unit.
            if let Some(ref hooking_unit) = self.units[u].hooking_unit {
                match self.unit_with_id(hooking_unit) {
                    Some(h) => {
                        let distance = self.units[h].position - self.units[u].position;
                        let speed = self.units[u].speed();

                        if distance.norm() > 32.0 {
                            self.units[h].position -= distance.normalize() * speed;
                        }
                    }
                    None => {
                        // The unit disappeared! Stop hooking it.
                        self.units[u].hooking_unit = None;
                    }
                }
            }

            // Units that are hooked cannot act.
            if self.units[u].being_hooked_by.is_some() {
                continue;
            }

            // Move on conveyors.
            let tile = self.game_map.to_tile(self.units[u].position);
            if let TileRole::Conveyor { direction } = self.game_map.tile(tile.0, tile.1).role {
                self.units[u].position += direction.to_vector() * 0.7;
            }

            let (direction, target) = {
                let unit = &mut self.units[u];

                // Check direct line of sight, otherwise use the pre-computed Dijkstra flow
                if let (Some(target), Some(flow)) = (&unit.target, &unit.path) {
                    if self
                        .game_map
                        .raycast_to_target(unit, unit.position, *target)
                    {
                        ((*target - unit.position).normalize(), target.clone())
                    } else {
                        match interpolate_flow(flow.as_ref(), unit.position) {
                            Some(flow_direction) => (flow_direction.clone(), target.clone()),
                            None => {
                                unit.intended_direction = None;
                                unit.target = None;
                                unit.path = None;
                                continue;
                            }
                        }
                    }
                } else {
                    continue;
                }
            };

            // Check for non-permanent obstacles (other units)
            // ???

            match self.units[u].action {
                // Mine
                UnitAction::Mining {
                    tile,
                    active: _,
                    animation,
                } => {
                    let unit = &mut self.units[u];

                    let tile_position = tile_center(tile);
                    let distance = (unit.position - tile_position).norm();

                    if distance < 64.0 {
                        unit.intended_direction = None;

                        unit.action = UnitAction::Mining {
                            tile,
                            active: true,
                            animation: (animation + 1) % 10,
                        };

                        let tile = self.game_map.tile_mut(tile);
                        if tile.role == TileRole::Rock {
                            tile.mining_progress += 1;
                            if tile.mining_progress > 80 {
                                tile.mining_progress = 0;
                                tile.role = TileRole::Ground;

                                create_units.push(Unit {
                                    id: UnitId::default(),
                                    position: tile_position,
                                    rotation: Vector2::new(0.0, -1.0),
                                    intended_direction: None,
                                    selected: false,
                                    target: None,
                                    path: None,
                                    role: UnitRole::Ore,
                                    action: UnitAction::Normal,
                                    hooking_unit: None,
                                    being_hooked_by: None,
                                })
                            }
                        } else {
                            unit.action = UnitAction::Normal;
                            unit.target = None;
                            unit.path = None;
                        }
                        continue;
                    } else {
                        unit.action = UnitAction::Mining {
                            tile,
                            active: false,
                            animation: 0,
                        };
                    }
                }

                // Build.
                UnitAction::Building { tile } => {
                    let unit = &mut self.units[u];

                    let distance = (unit.position - tile_center(tile)).norm();

                    if distance < 64.0 {
                        let tile = self.game_map.tile_mut(tile);
                        if let Some(build_plan) = tile.build_plan {
                            tile.role = build_plan;
                            tile.build_plan = None;
                        }

                        unit.action = UnitAction::Normal;
                        unit.target = None;
                        unit.path = None;
                        unit.intended_direction = None;

                        continue;
                    }
                }

                // Go hook something.
                UnitAction::StartHooking { ref hook_unit } => {
                    let hooking_unit = hook_unit.clone();

                    let stop_unit = match self.unit_with_id(hook_unit) {
                        Some(h) => {
                            let distance = (self.units[u].position - self.units[h].position).norm();

                            if let Some(ref already_hooked_by_unit) = self.units[h].being_hooked_by
                            {
                                if let Some(u2) = self.unit_with_id(already_hooked_by_unit) {
                                    self.units[u2].hooking_unit = None;
                                }
                            }

                            if distance < 64.0 {
                                self.units[u].action = UnitAction::Normal;
                                if let Some(ref already_hooking_unit) = self.units[u].hooking_unit {
                                    if let Some(h2) = self.unit_with_id(already_hooking_unit) {
                                        self.units[h2].being_hooked_by = None;
                                    }
                                }
                                self.units[u].hooking_unit = Some(hooking_unit);

                                self.units[h].being_hooked_by = Some(self.units[u].id.clone());

                                true
                            } else {
                                false
                            }
                        }
                        None => {
                            // The unit disappeared! Stop hooking it.
                            self.units[u].action = UnitAction::Normal;
                            true
                        }
                    };

                    if stop_unit {
                        let unit = &mut self.units[u];
                        unit.target = None;
                        unit.path = None;
                        unit.intended_direction = None;
                        continue;
                    }
                }

                UnitAction::StopHooking => {
                    if let Some(ref hooking_unit) = self.units[u].hooking_unit {
                        match self.unit_with_id(hooking_unit) {
                            Some(h) => {
                                self.units[h].being_hooked_by = None;
                            }
                            None => (),
                        }
                    }

                    let unit = &mut self.units[u];
                    unit.hooking_unit = None;
                    unit.target = None;
                    unit.path = None;
                    unit.intended_direction = None;
                    continue;
                }

                // Do nothing special
                UnitAction::Normal => (),
            }

            let unit = &mut self.units[u];

            // Move.
            let path = target - unit.position;
            if path.norm() > 10.0 {
                unit.intended_direction = Some(direction);
            } else {
                unit.intended_direction = None;
                unit.target = None;
                unit.path = None;
            }
        }

        for unit in self.units.iter_mut() {
            if let Some(direction) = unit.intended_direction {
                unit.rotation = direction;
                unit.position += direction * unit.speed();
            }
        }

        self.units.extend(create_units);

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult<()> {
        graphics::clear(ctx, (0.2, 0.2, self.focus_color).into());

        // Draw ground
        let mut sand = graphics::spritebatch::SpriteBatch::new(self.sand_image.clone());
        let mut rocks: Vec<_> = (0..self.rock_images.len())
            .map(|mining_level| {
                graphics::spritebatch::SpriteBatch::new(self.rock_images[mining_level].clone())
            })
            .collect();

        for x in 0..((self.window_size.x / 32.0) as usize) {
            for y in 0..((self.window_size.y / 32.0) as usize) {
                let draw_param =
                    graphics::DrawParam::new().dest(Point2::new(x as f32 * 32.0, y as f32 * 32.0));

                sand.add(draw_param);

                if !self.game_map.tile(x, y).passable() {
                    let mining_level = self.game_map.tile(x, y).mining_progress / 10;
                    rocks[mining_level.min(6) as usize].add(draw_param);
                }
            }
        }

        graphics::draw(ctx, &sand, graphics::DrawParam::default())?;
        for rocks in rocks {
            graphics::draw(ctx, &rocks, graphics::DrawParam::default())?;
        }

        // Draw structures and build plans
        for x in 0..((self.window_size.x / 32.0) as usize) {
            for y in 0..((self.window_size.y / 32.0) as usize) {
                let tile = self.game_map.tile(x, y);
                match tile.role {
                    TileRole::Ground => (),
                    TileRole::Rock => (),
                    TileRole::Conveyor { direction } => {
                        let connected_tile =
                            self.game_map.tile_in_direction((x, y), direction.next());
                        let draw_curved =
                            self.game_map.tile(connected_tile.0, connected_tile.1).role
                                == TileRole::Conveyor {
                                    direction: direction.next().next().next(),
                                };
                        let direction = if draw_curved {
                            direction.next().next()
                        } else {
                            direction
                        };

                        let draw_param = graphics::DrawParam::new()
                            .offset(Point2::new(0.5, 0.5))
                            .rotation(to_angle(direction.to_vector()))
                            .dest(tile_center((x, y)));
                        let image = if draw_curved {
                            &self.conveyor_curved_images[self.conveyor_animation / 3]
                        } else {
                            &self.conveyor_straight_images[self.conveyor_animation / 3]
                        };
                        graphics::draw(ctx, image, draw_param)?;
                    }
                }

                match tile.build_plan {
                    Some(TileRole::Conveyor { direction }) => {
                        let draw_param = graphics::DrawParam::new()
                            .offset(Point2::new(0.5, 0.5))
                            .rotation(to_angle(direction.to_vector()))
                            .dest(tile_center((x, y)))
                            .color((1.0, 1.0, 1.0, 0.3).into());
                        graphics::draw(ctx, &self.conveyor_ghost_image, draw_param)?;
                    }
                    _ => (),
                }
            }
        }

        // Draw units
        for unit in self.units.iter() {
            self.draw_unit(ctx, unit)?;

            if unit.selected {
                draw_circle(ctx, unit.position, 20.0, (0.6, 0.6, 0.8, 0.6).into(), false)?;
            }

            if let Some(target) = unit.target {
                let mesh = graphics::Mesh::new_line(
                    ctx,
                    &[unit.position, target],
                    1.0,
                    (0.0, 0.0, 0.0, 0.5).into(),
                )?;

                graphics::draw(ctx, &mesh, graphics::DrawParam::default())?;
            }
        }

        // Draw selections
        if let Some((p1, p2)) = self.selecting {
            let (point, size) = to_rect(p1, p2);

            let rect = graphics::Rect::new(point.x, point.y, size.x, size.y);
            for (color, fill) in &[
                ((0.0, 0.0, 0.0, 0.2).into(), graphics::DrawMode::fill()),
                (graphics::BLACK, graphics::DrawMode::stroke(1.0)),
            ] {
                let mesh = graphics::Mesh::new_rectangle(ctx, *fill, rect, graphics::BLACK)?;
                graphics::draw(ctx, &mesh, graphics::DrawParam::default().color(*color))?;
            }

            for unit in self
                .units
                .iter()
                .filter(|unit| unit.selected_by(point, size))
            {
                draw_circle(ctx, unit.position, 20.0, graphics::BLACK, false)?;
            }
        } else {
            if let Some(pointing_at_unit) = self
                .units
                .iter()
                .filter(|unit| Some(&unit.id) == self.pointing_at_unit.as_ref())
                .nth(0)
            {
                draw_circle(
                    ctx,
                    pointing_at_unit.position,
                    20.0,
                    (0.0, 0.0, 0.0, 0.3).into(),
                    false,
                )?;
            }
        }

        // Draw action intents
        let selected_worker = self
            .units
            .iter()
            .filter(|unit| unit.role == UnitRole::Worker)
            .any(|unit| unit.selected);

        if selected_worker {
            if self.pointing_at(TileRole::Rock) {
                let draw_param = graphics::DrawParam::new()
                    .offset(Point2::new(0.5, 0.5))
                    .dest(tile_center(self.pointing_at_tile));
                graphics::draw(ctx, &self.mine_image, draw_param)?;
            } else if let Some(pointing_at_unit) = self.pointing_at_unit() {
                let draw_param = graphics::DrawParam::new()
                    .offset(Point2::new(0.5, 0.5))
                    .dest(pointing_at_unit.position);
                graphics::draw(ctx, &self.link_image, draw_param)?;
            }
        }

        // Draw build ghosts
        if let Some(TileRole::Conveyor { direction }) = self.build_plan {
            if self.pointing_at(TileRole::Ground) {
                let draw_param = graphics::DrawParam::new()
                    .offset(Point2::new(0.5, 0.5))
                    .rotation(to_angle(direction.to_vector()))
                    .dest(tile_center(self.pointing_at_tile))
                    .color((1.0, 1.0, 1.0, 0.5).into());
                graphics::draw(ctx, &self.conveyor_ghost_image, draw_param)?;
            }
        }

        // Draw mining effects
        for unit in self.units.iter() {
            if let UnitAction::Mining {
                tile,
                active: true,
                animation,
            } = unit.action
            {
                let tile_position = tile_center(tile);

                let animation = 2.0 + (animation as i8 - 5).abs() as f32 / 3.0;

                if (tile_position - unit.position).norm() < 64.0 {
                    let mesh = graphics::Mesh::new_line(
                        ctx,
                        &[unit.position, tile_position],
                        animation,
                        (0.5, 0.5, 0.0).into(),
                    )?;
                    graphics::draw(ctx, &mesh, graphics::DrawParam::default())?;

                    let mesh = graphics::Mesh::new_circle(
                        ctx,
                        graphics::DrawMode::fill(),
                        tile_position,
                        animation * 3.0,
                        3.0,
                        (0.5, 0.5, 0.0).into(),
                    )?;
                    graphics::draw(ctx, &mesh, graphics::DrawParam::default())?;
                }
            }
        }

        // Draw hooking/towing effects
        for u in 0..self.units.len() {
            if let Some(ref hooking_unit) = self.units[u].hooking_unit {
                if let Some(h) = self.unit_with_id(hooking_unit) {
                    let p1 = self.units[u].position;
                    let p2 = self.units[h].position;

                    let mesh =
                        graphics::Mesh::new_line(ctx, &[p1, p2], 2.0, (0.4, 0.4, 1.0, 0.5).into())?;
                    graphics::draw(ctx, &mesh, graphics::DrawParam::default())?;
                }
            }
        }

        graphics::present(ctx)?;

        Ok(())
    }

    fn mouse_button_down_event(&mut self, _ctx: &mut Context, button: MouseButton, x: f32, y: f32) {
        if button == MouseButton::Left && x > 0.0 && y > 0.0 {
            if self.build_plan.is_some() && self.pointing_at(TileRole::Ground) {
                self.game_map.tile_mut(self.pointing_at_tile).build_plan = self.build_plan;
            } else {
                self.selecting = Some((Point2::new(x, y), Point2::new(x, y)));
            }
        } else if button == MouseButton::Right {
            let mut target = Point2::new(x, y);
            let tile = self.game_map.to_tile(target);

            let path = Rc::new(self.game_map.path_flow_to_target(target));
            let pointing_at_rock = self.pointing_at(TileRole::Rock);
            let pointing_at_build_plan = self.game_map.tile(tile.0, tile.1).build_plan;

            if pointing_at_rock {
                target = tile_center(tile);
            } else if let Some(pointing_at_unit) = self.pointing_at_unit() {
                target = pointing_at_unit.position;
            } else if pointing_at_build_plan.is_some() {
                target = tile_center(tile);
            }

            for selected_unit in self.units.iter_mut().filter(|unit| unit.selected) {
                if selected_unit.role == UnitRole::Worker {
                    if pointing_at_rock {
                        selected_unit.action = UnitAction::Mining {
                            tile,
                            active: false,
                            animation: 0,
                        };
                    } else if let Some(ref pointing_at_unit) = self.pointing_at_unit {
                        if pointing_at_unit == &selected_unit.id {
                            // Don't hook self.
                            selected_unit.action = UnitAction::Normal;
                        } else {
                            if selected_unit.hooking_unit.as_ref() == Some(pointing_at_unit) {
                                selected_unit.action = UnitAction::StopHooking;
                            } else {
                                selected_unit.action = UnitAction::StartHooking {
                                    hook_unit: pointing_at_unit.clone(),
                                };
                            }
                        }
                    } else if pointing_at_build_plan.is_some() {
                        selected_unit.action = UnitAction::Building { tile };
                    } else {
                        selected_unit.action = UnitAction::Normal;
                    }
                }

                selected_unit.target = Some(target);
                selected_unit.path = Some(path.clone());
            }
        }
    }

    fn mouse_button_up_event(&mut self, _ctx: &mut Context, button: MouseButton, _x: f32, _y: f32) {
        if button == MouseButton::Left {
            if let Some((p1, p2)) = self.selecting.take() {
                if (p1 - p2).norm() > 3.0 {
                    // Drag-select.
                    let (point, size) = to_rect(p1, p2);
                    for unit in self.units.iter_mut() {
                        unit.selected = unit.selected_by(point, size);
                    }
                } else {
                    // Click-select.
                    for unit in self.units.iter_mut() {
                        if let Some(ref pointing_at_unit) = self.pointing_at_unit {
                            unit.selected = &unit.id == pointing_at_unit;
                        } else {
                            unit.selected = false;
                        }
                    }
                }
            }
        }
    }

    fn mouse_motion_event(&mut self, _ctx: &mut Context, x: f32, y: f32, _dx: f32, _dy: f32) {
        let pointing_at_point = Point2::new(x, y);

        if let Some((p1, _)) = self.selecting {
            self.selecting = Some((p1, pointing_at_point));
        }

        self.pointing_at_tile = self.game_map.to_tile(pointing_at_point);

        self.pointing_at_unit = None;

        for unit in self.units.iter() {
            if (unit.position - pointing_at_point).norm() < 24.0 {
                self.pointing_at_unit = Some(unit.id.clone());
            }
        }
    }

    fn mouse_wheel_event(&mut self, _ctx: &mut Context, _x: f32, _y: f32) {
        if let Some(TileRole::Conveyor { direction }) = self.build_plan {
            self.build_plan = Some(TileRole::Conveyor {
                direction: direction.next(),
            });
        }
    }

    fn key_down_event(
        &mut self,
        ctx: &mut Context,
        keycode: ggez::input::keyboard::KeyCode,
        _keymods: ggez::input::keyboard::KeyMods,
        _repeat: bool,
    ) {
        if keycode == ggez::input::keyboard::KeyCode::Escape {
            ggez::quit(ctx);
        } else if keycode == ggez::input::keyboard::KeyCode::B {
            self.build_plan = match self.build_plan {
                None => Some(TileRole::Conveyor {
                    direction: Direction::Up,
                }),
                Some(_) => None,
            };
        }
    }

    fn focus_event(&mut self, _ctx: &mut Context, gained: bool) {
        self.focused = gained;
        self.selecting = None;
    }

    fn resize_event(&mut self, _ctx: &mut Context, width: f32, height: f32) {
        self.window_size = Vector2::new(width, height);
    }
}

fn main() -> GameResult {
    let (mut ctx, mut event_loop) = ContextBuilder::new("MyId", "MyAuthor")
        .window_setup(ggez::conf::WindowSetup {
            title: "Tile RTS".into(),
            ..Default::default()
        })
        .window_mode(ggez::conf::WindowMode {
            width: 1024.0,
            height: 768.0,
            resizable: false,
            ..Default::default()
        })
        .add_resource_path(std::path::PathBuf::from("./resources"))
        .build()?;

    let mut my_game = MyGame::new(&mut ctx)?;

    my_game.units.push(Unit {
        id: UnitId::default(),
        position: Point2::new(200.0, 200.0),
        rotation: Vector2::new(0.0, -1.0),
        intended_direction: None,
        selected: false,
        target: None,
        path: None,
        role: UnitRole::Worker,
        action: UnitAction::Normal,
        hooking_unit: None,
        being_hooked_by: None,
    });

    my_game.units.push(Unit {
        id: UnitId::default(),
        position: Point2::new(300.0, 600.0),
        rotation: Vector2::new(0.0, -1.0),
        intended_direction: None,
        selected: false,
        target: None,
        path: None,
        role: UnitRole::Worker,
        action: UnitAction::Normal,
        hooking_unit: None,
        being_hooked_by: None,
    });

    event::run(&mut ctx, &mut event_loop, &mut my_game)?;

    Ok(())
}
