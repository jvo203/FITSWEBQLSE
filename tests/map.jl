a = rand(Float32, 5, 5)

function test(x, inc)
    return x + inc
end

function invalidate(x, threshold)::Bool
    x > threshold ? true : false
end

b = test.(a, 2)
c = map(x -> test(x, 2), a)

println(c)

d = invalidate.(a, 0.9)
e = map(x -> invalidate(x, 0.9), a)

println(a)
println(d)
println(e)

function invalidate2(x, datamin, datamax, ignrval)::Bool
    val = Float32(x)

    !isfinite(val) || (val < datamin) || (val > datamax) || (val <= ignrval)
end

f = map(x -> invalidate2(x, 0.3, 0.7, 0.0), a)

println(f)