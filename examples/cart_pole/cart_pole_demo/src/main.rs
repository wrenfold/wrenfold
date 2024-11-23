//! wrenfold symbolic code generator.
//! Copyright (c) 2024 Gareth Cross
//! For license information refer to accompanying LICENSE file.
use egui::{pos2, vec2, Color32, Rect, Shape, Stroke, Ui};
use nalgebra as na;
use rand::prelude::Distribution;

mod generated;

// See `CartPoleParamsSymbolic` for the meaning of these variables.
#[derive(Debug, Clone, Copy)]
pub struct CartPoleParams {
    pub m_b: f64,
    pub m_1: f64,
    pub m_2: f64,
    pub l_1: f64,
    pub l_2: f64,
    pub g: f64,
    pub mu_b: f64,
    pub v_mu_b: f64,
    pub c_d: f64,
    pub x_s: f64,
    pub k_s: f64,
}

impl Default for CartPoleParams {
    fn default() -> Self {
        Self {
            m_b: 1.0,
            m_1: 0.25,
            m_2: 0.20,
            l_1: 0.30,
            l_2: 0.15,
            g: 9.81,
            mu_b: 0.1,
            v_mu_b: 0.1,
            c_d: 0.02,
            x_s: 1.5,
            k_s: 500.0,
        }
    }
}

fn runge_kutta_4th_order<const DIM: usize, F>(
    x: &na::SMatrix<f64, DIM, 1>,
    h: f64,
    f: F,
) -> na::SMatrix<f64, DIM, 1>
where
    F: Fn(&na::SMatrix<f64, DIM, 1>) -> na::SMatrix<f64, DIM, 1>,
{
    let k1 = f(x);
    let k2 = f(&(x + k1 * h / 2.0));
    let k3 = f(&(x + k2 * h / 2.0));
    let k4 = f(&(x + k3 * h));
    x + (h / 6.0) * (k1 + k2 * 2.0 + k3 * 2.0 + k4)
}

/// Main struct of the app.
pub struct App {
    params: CartPoleParams,
    state: na::Vector6<f64>,
    sim_rate: f64,
    previous_update_time: Option<std::time::Instant>,
}

impl App {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        egui_extras::install_image_loaders(&cc.egui_ctx);
        Self {
            params: Default::default(),
            state: na::Vector6::zeros(),
            sim_rate: 1.0,
            previous_update_time: None,
        }
    }

    /// Step the simulator forward.
    fn step_sim(&mut self) {
        let current_time = std::time::Instant::now();
        if let Some(previous_update_time) = self.previous_update_time {
            let mut dt = current_time
                .duration_since(previous_update_time)
                .as_secs_f64()
                * self.sim_rate;

            // Break interval into sub-intervals to improve integration accuracy.
            const INTERNAL_DT: f64 = 0.0001;
            while dt > 0.0 {
                self.sub_step_sim(dt.min(INTERNAL_DT));
                dt -= INTERNAL_DT;
            }
        }
        self.previous_update_time = Some(current_time);
    }

    fn sub_step_sim(&mut self, dt: f64) {
        self.state = runge_kutta_4th_order(&self.state, dt, |x| {
            let mut x_dot = na::Vector6::zeros();
            generated::cart_double_pole_dynamics(
                &self.params,
                x,
                &mut x_dot,
                Option::<&mut f64>::None,
                Option::<&mut na::Matrix6<f64>>::None,
            );
            x_dot
        });
    }

    /// Randomize the initial conditions.
    fn reset_state(&mut self) {
        let dist = rand::distributions::Uniform::new(-std::f64::consts::PI, std::f64::consts::PI);
        let mut rng = rand::thread_rng();
        self.state[0] = 0.0; //  initial position
        self.state[1] = dist.sample(&mut rng);
        self.state[2] = dist.sample(&mut rng);
        self.state[3] = 0.0; // initial velocities:
        self.state[4] = 0.0;
        self.state[5] = 0.0;
    }

    /// Draw controls for adjusting spline order.
    fn draw_controls(&mut self, ui: &mut Ui) {
        ui.horizontal(|ui| {
            ui.vertical(|ui| {
                ui.horizontal(|ui| {
                    ui.monospace("Sim rate:");
                    ui.add(egui::Slider::new(&mut self.sim_rate, 0.01..=1.0));
                });
                if ui.button("Reset").clicked() {
                    self.reset_state();
                }
            });
            ui.separator();
            ui.vertical(|ui| {
                ui.horizontal(|ui| {
                    ui.monospace("m_b:");
                    ui.add(egui::Slider::new(&mut self.params.m_b, 0.1..=2.0));
                });
                ui.horizontal(|ui| {
                    ui.monospace("m_1:");
                    ui.add(egui::Slider::new(&mut self.params.m_1, 0.1..=2.0));
                });
                ui.horizontal(|ui| {
                    ui.monospace("m_2:");
                    ui.add(egui::Slider::new(&mut self.params.m_2, 0.1..=2.0));
                });
                ui.horizontal(|ui| {
                    ui.monospace("l_1:");
                    ui.add(egui::Slider::new(&mut self.params.l_1, 0.1..=1.0));
                });
                ui.horizontal(|ui| {
                    ui.monospace("l_2:");
                    ui.add(egui::Slider::new(&mut self.params.l_2, 0.1..=1.0));
                });
            });
            ui.separator();
            ui.vertical(|ui| {
                ui.horizontal(|ui| {
                    ui.monospace("mu_b:");
                    ui.add(egui::Slider::new(&mut self.params.mu_b, 0.01..=1.0));
                });
                ui.horizontal(|ui| {
                    ui.monospace("c_d:");
                    ui.add(egui::Slider::new(&mut self.params.c_d, 0.01..=1.0));
                });
                ui.horizontal(|ui| {
                    ui.monospace("k_s:");
                    ui.add(egui::Slider::new(&mut self.params.k_s, 100.0..=1000.0));
                });
            });
        });
    }

    fn draw_cart_pole(&self, ui: &mut Ui) {
        egui::Frame::canvas(ui.style()).show(ui, |ui| {
            let available_width = ui.available_width();
            let fixed_height = ui.available_height();

            let (response, painter) =
                ui.allocate_painter(vec2(available_width, fixed_height), egui::Sense::hover());

            // Dimensions of the viewport in meters:
            let vp_height = 2.0;
            let vp_width = (available_width / fixed_height) * vp_height;

            let to_screen = emath::RectTransform::from_to(
                Rect::from_min_size(egui::Pos2::ZERO, vec2(vp_width, vp_height)),
                response.rect,
            );

            // Draw the background:
            painter.add(Shape::rect_filled(
                response.rect.expand(5.0),
                egui::Rounding::ZERO,
                Color32::from_rgb(30, 41, 59),
            ));

            const CART_WIDTH: f32 = 0.30;
            const CART_HEIGHT: f32 = 0.20;
            const MASS_RADIUS: f32 = 0.025;

            let origin = pos2(vp_width / 2.0, vp_height / 2.0);

            // Draw the ground:
            painter.add(Shape::dashed_line(
                &[
                    to_screen.transform_pos(pos2(0.0, origin.y + CART_HEIGHT / 2.0)),
                    to_screen.transform_pos(pos2(vp_width, origin.y + CART_HEIGHT / 2.0)),
                ],
                Stroke::new(2.0, Color32::from_rgb(251, 161, 8)),
                5.0,
                5.0,
            ));

            // Draw spring boundaries:
            painter.add(Shape::dashed_line(
                &[
                    to_screen.transform_pos(pos2(-self.params.x_s as f32 + origin.x, 0.0)),
                    to_screen.transform_pos(pos2(-self.params.x_s as f32 + origin.x, vp_height)),
                ],
                Stroke::new(1.0, Color32::from_rgb(232, 158, 184)),
                5.0,
                5.0,
            ));
            painter.add(Shape::dashed_line(
                &[
                    to_screen.transform_pos(pos2(self.params.x_s as f32 + origin.x, 0.0)),
                    to_screen.transform_pos(pos2(self.params.x_s as f32 + origin.x, vp_height)),
                ],
                Stroke::new(1.0, Color32::from_rgb(232, 158, 184)),
                5.0,
                5.0,
            ));

            // Draw the cart:
            let p_base = origin + vec2(self.state[0] as f32, 0.0);
            let cart_rect = Rect::from_center_size(
                to_screen.transform_pos(p_base),
                to_screen.scale() * vec2(CART_WIDTH, CART_HEIGHT),
            );
            painter.add(Shape::rect_filled(
                cart_rect,
                5.0,
                Color32::from_rgb(15, 118, 110),
            ));
            painter.add(Shape::rect_stroke(
                cart_rect,
                5.0,
                Stroke::new(2.0, Color32::from_rgb(229, 229, 229)),
            ));

            // Draw the poles:
            let p_m1 = p_base
                + vec2(
                    (self.state[1].cos() * self.params.l_1) as f32,
                    (-self.state[1].sin() * self.params.l_1) as f32,
                );
            let p_m2 = p_m1
                + vec2(
                    (self.state[2].cos() * self.params.l_2) as f32,
                    (-self.state[2].sin() * self.params.l_2) as f32,
                );

            painter.add(Shape::line_segment(
                [
                    to_screen.transform_pos(p_base),
                    to_screen.transform_pos(p_m1),
                ],
                Stroke::new(2.0, Color32::from_rgb(229, 229, 229)),
            ));
            painter.add(Shape::line_segment(
                [to_screen.transform_pos(p_m1), to_screen.transform_pos(p_m2)],
                Stroke::new(2.0, Color32::from_rgb(229, 229, 229)),
            ));

            // Draw point masses:
            painter.add(Shape::circle_filled(
                to_screen.transform_pos(p_m1),
                to_screen.scale().x * MASS_RADIUS,
                Color32::from_rgb(195, 57, 27),
            ));
            painter.add(Shape::circle_stroke(
                to_screen.transform_pos(p_m1),
                to_screen.scale().x * MASS_RADIUS,
                Stroke::new(2.0, Color32::from_rgb(229, 229, 229)),
            ));
            painter.add(Shape::circle_filled(
                to_screen.transform_pos(p_m2),
                to_screen.scale().x * MASS_RADIUS,
                Color32::from_rgb(195, 57, 27),
            ));
            painter.add(Shape::circle_stroke(
                to_screen.transform_pos(p_m2),
                to_screen.scale().x * MASS_RADIUS,
                Stroke::new(2.0, Color32::from_rgb(229, 229, 229)),
            ));
        });
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.step_sim();

        egui::TopBottomPanel::top("top_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.label("Cart Pole Demo");
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical(|ui| {
                self.draw_controls(ui);
                ui.separator();
                ui.monospace(
                    "The vertical pink lines denote the position of the boundary springs.",
                );
                ui.monospace("See cart_pole_dynamics.py for the meaning of the parameters.");
                self.draw_cart_pole(ui);
            });
        });

        ctx.request_repaint_after(std::time::Duration::from_secs_f64(1.0 / 30.0));
    }
}

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([1280.0, 720.0])
            .with_drag_and_drop(true),
        ..Default::default()
    };
    eframe::run_native(
        "Cart Pole Demo",
        options,
        Box::new(
            |cc| -> Result<_, Box<dyn std::error::Error + Send + Sync>> {
                Ok(Box::new(App::new(cc)))
            },
        ),
    )
}
