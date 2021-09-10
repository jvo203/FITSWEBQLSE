mutable struct KalmanFilter
    Float64::estimate_position
    Float64::estimate_velocity
    Float64::p_xx
    Float64::p_xv
    Float64::p_vv
    Float64::position_variance
    Float64::velocity_variance
    Float64::r
    Bool::has_velocity


end
