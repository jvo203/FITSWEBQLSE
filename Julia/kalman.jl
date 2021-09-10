mutable struct KalmanFilter
    estimate_position::Float64
    estimate_velocity::Float64
    p_xx::Float64
    p_xv::Float64
    p_vv::Float64
    position_variance::Float64
    velocity_variance::Float64
    r::Float64
    has_velocity::Bool

    function KalmanFilter()
        return KalmanFilter(0.0, false)
    end

    function KalmanFilter(position::Float64, video::Bool)
        local estimate_position, estimate_velocity, has_velocity
        local p_xx, p_xv, p_vv
        local position_variance, velocity_variance, r

        estimate_position = position
        estimate_velocity = 0.0
        has_velocity = false

        if !video
            # normal settings used for tracking X-Y coordinates
            p_xx = 0.1
            p_xv = 0.1
            p_vv = 0.1
            position_variance = 0.01
            velocity_variance = 0.01
            r = 100000.0
        else
            # special setting used by video streaming
            p_xx = 0.1 * position
            p_xv = 1.0
            p_vv = 1.0
            position_variance = 0.1 * position
            velocity_variance = 0.01 * position / 1000.0
            r = 0.01 * position
        end

        return new(
            estimate_position,
            estimate_velocity,
            p_xx,
            p_xv,
            p_vv,
            position_variance,
            velocity_variance,
            r,
            has_velocity,
        )
    end
end

function reset(filter::KalmanFilter, position::Float64)
    filter.estimate_position = position
    filter.estimate_velocity = 0.0
    filter.has_velocity = false
end