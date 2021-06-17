using Distributed;

nheads = @distributed (+) for i = 1:200000000
    Int(rand(Bool))
end

println("nheads = $nheads")