//! wrenfold symbolic code generator.
//! Copyright (c) 2024 Gareth Cross
//! For license information refer to accompanying LICENSE file.
use std::collections::HashMap;

use arrayvec::ArrayVec;
use egui::{Color32, Slider, Ui};
use egui_plot::{Line, Plot, PlotPoints};
use nalgebra as na;

use bspline_rust_test as bsplines;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum SplineType {
    /// Ordinary b-spline.
    Ordinary,
    /// Cumulative b-spline.
    Cumulative,
}

type SplineFunction = Box<dyn Fn(f64, usize, usize) -> (usize, ArrayVec<f64, 6>)>;

/// Main struct of the app.
pub struct App {
    /// The selected order.
    order: usize,
    /// Number of knots to render in our demo spline.
    num_knots: usize,
    /// Take the n'th derivative.
    derivative: usize,
    /// Whether to draw the cumulative b-spline.
    cumulative: bool,
    /// Map from order to a function that evaluates the bspline.
    /// Function signature is [x-value, number of knots, derivative order].
    splines: HashMap<(SplineType, usize), SplineFunction>,
}

impl App {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        egui_extras::install_image_loaders(&cc.egui_ctx);

        let mut this = Self {
            order: 3,
            num_knots: 11,
            derivative: 0,
            cumulative: false,
            splines: HashMap::default(),
        };
        use SplineType::*;
        this.add_spline(Ordinary, bsplines::eval_bspline_coefficients_order3);
        this.add_spline(Ordinary, bsplines::eval_bspline_coefficients_order4);
        this.add_spline(Ordinary, bsplines::eval_bspline_coefficients_order5);
        this.add_spline(Ordinary, bsplines::eval_bspline_coefficients_order6);
        this.add_spline(
            Cumulative,
            bsplines::eval_cumulative_bspline_coefficients_order3,
        );
        this.add_spline(
            Cumulative,
            bsplines::eval_cumulative_bspline_coefficients_order4,
        );
        this.add_spline(
            Cumulative,
            bsplines::eval_cumulative_bspline_coefficients_order5,
        );
        this.add_spline(
            Cumulative,
            bsplines::eval_cumulative_bspline_coefficients_order6,
        );
        this
    }

    /// Add a new function to the `splines`` map.
    fn add_spline<const ORDER: usize, const ORDER_MINUS_ONE: usize, F>(
        &mut self,
        spline_type: SplineType,
        f: F,
    ) where
        F: Fn(f64, usize, &mut na::SMatrix<f64, ORDER, ORDER_MINUS_ONE>) -> usize + 'static,
    {
        let func =
            move |t: f64, num_knots: usize, derivative: usize| -> (usize, ArrayVec<f64, 6>) {
                let mut coeffs = na::SMatrix::zeros();
                let index = f(t, num_knots, &mut coeffs);
                (
                    index,
                    coeffs
                        .fixed_columns::<1>(derivative)
                        .iter()
                        .copied()
                        .collect(),
                )
            };
        self.splines.insert((spline_type, ORDER), Box::new(func));
    }

    /// Draw controls for adjusting spline order.
    fn draw_controls(&mut self, ui: &mut Ui) {
        ui.horizontal(|ui| {
            ui.monospace("Order:");
            if ui.add(Slider::new(&mut self.order, 3..=6)).changed() {
                self.num_knots = self.num_knots.max(self.order + 1);
                self.derivative = self.derivative.min(self.order - 2);
            }
        });
        ui.horizontal(|ui| {
            ui.monospace("Number of knots:");
            ui.add(Slider::new(&mut self.num_knots, (self.order + 1)..=20));
        });
        ui.horizontal(|ui| {
            ui.monospace("Derivative order:");
            ui.add(Slider::new(&mut self.derivative, 0..=(self.order - 2)));
        });
        ui.horizontal(|ui| {
            ui.monospace("Cumulative:");
            ui.checkbox(&mut self.cumulative, "Enabled");
        });
    }

    fn draw_plot(&self, ui: &mut Ui) {
        let mut plot = Plot::new("plot").show_axes(true).show_grid(true);
        plot = plot
            .view_aspect(1.0)
            .width(ui.available_width())
            .height(ui.available_height())
            .allow_scroll(false)
            .allow_boxed_zoom(false)
            .legend(egui_plot::Legend::default());

        const NUM_PTS: usize = 1000;
        plot.show(ui, |plot_ui| {
            let mut pts_per_poly: HashMap<usize, Vec<[f64; 2]>> =
                HashMap::with_capacity(self.num_knots + self.order);

            let key = if self.cumulative {
                (SplineType::Cumulative, self.order)
            } else {
                (SplineType::Ordinary, self.order)
            };
            let spline_func = self.splines.get(&key).unwrap();

            for i in 0..NUM_PTS {
                let t = i as f64 / (NUM_PTS as f64 - 1.0);
                let (index, values) = spline_func(t, self.num_knots, self.derivative);

                for (j, v) in values.into_iter().enumerate() {
                    pts_per_poly.entry(index + j).or_default().push([t, v]);
                }
            }

            let num_polys = pts_per_poly.len();
            let grad = colorgrad::rainbow();
            for (poly_index, values) in pts_per_poly.into_iter() {
                let rgba = grad.at((poly_index as f64 + 0.5) / num_polys as f64);
                let color = Color32::from_rgb(
                    (rgba.r * 255.0) as u8,
                    (rgba.g * 255.0) as u8,
                    (rgba.b * 255.0) as u8,
                );

                let line = Line::new(PlotPoints::from(values))
                    .color(color)
                    .style(egui_plot::LineStyle::Solid)
                    .name(format!("Poly {:02}", poly_index));
                plot_ui.line(line);
            }
        });
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.label("Spline Demo");
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical(|ui| {
                self.draw_controls(ui);
                ui.separator();
                self.draw_plot(ui);
            });
        });
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
        "BSpline Demo",
        options,
        Box::new(
            |cc| -> Result<_, Box<dyn std::error::Error + Send + Sync>> {
                Ok(Box::new(App::new(cc)))
            },
        ),
    )
}
