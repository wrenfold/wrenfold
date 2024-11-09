//! wrenfold symbolic code generator.
//! Copyright (c) 2024 Gareth Cross

/// Used to test custom type support.
#[derive(Debug, Copy, Clone)]
pub struct Point2d {
    x: f64,
    y: f64,
}

impl Point2d {
    /// We customize the code-generation logic in the rust code generator so this constructor
    /// is invoked.
    pub fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    pub fn x(&self) -> f64 {
        self.x
    }

    pub fn y(&self) -> f64 {
        self.y
    }

    pub fn to_vector(&self) -> nalgebra::Vector2<f64> {
        nalgebra::Vector2::new(self.x, self.y)
    }
}

/// Use public properties to access `Circle`.
#[derive(Debug, Copy, Clone)]
pub struct Circle {
    pub center: Point2d,
    pub radius: f64,
}

impl Circle {
    pub fn to_vector(&self) -> nalgebra::Vector3<f64> {
        nalgebra::Vector3::new(self.center.x, self.center.y, self.radius)
    }
}

// Declare so we can check that nested_custom_type_2 compiles.
#[derive(Debug, Copy, Clone)]
pub struct FancyAggregateType {
    pub pt: Point2d,
    pub circle: Circle,
    pub matrix: nalgebra::Vector2<f64>,
    pub scalar: f64,
}

#[derive(Debug, Copy, Clone)]
pub struct MixedNumerics {
    pub value: f64,
    pub mode: i64,
}
