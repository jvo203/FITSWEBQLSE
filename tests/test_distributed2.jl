using Distributed;

println("nprocs: ", nprocs())
println(workers())

addprocs(1)
println(workers())

addprocs(1)
println(workers())

addprocs(["grid61"])
println(workers())

addprocs(["grid61"])
println(workers())

addprocs(["grid62"])
println(workers())

addprocs(["grid62"])
println(workers())

addprocs(["grid63"])
println(workers())

addprocs(["grid63"])
println(workers())