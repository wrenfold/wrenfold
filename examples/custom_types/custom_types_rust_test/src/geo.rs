use nalgebra as na;

#[derive(Debug, Copy, Clone)]
pub struct Pose3d {
    rotation: na::UnitQuaternion<f64>,
    translation: na::Vector3<f64>,
}

impl Pose3d {
    pub fn rotation(&self) -> &na::UnitQuaternion<f64> {
        &self.rotation
    }

    pub fn translation(&self) -> &na::Vector3<f64> {
        &self.translation
    }

    pub fn new(rotation: na::UnitQuaternion<f64>, translation: na::Vector3<f64>) -> Self {
        Self {
            rotation,
            translation,
        }
    }

    pub fn new_rotation_vector(
        rodrigues_vector: na::Vector3<f64>,
        translation: na::Vector3<f64>,
    ) -> Self {
        Self::new(na::UnitQuaternion::new(rodrigues_vector), translation)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Point3d {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Point3d {
    pub fn to_vector(&self) -> na::Vector3<f64> {
        na::vector![self.x, self.y, self.z]
    }
}
