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

println("distributed workers initialised, starting tests")

@everywhere function test()
    id = myid()
    println(id)

    try
        open("/data/cache/$(id).txt", "w") do io
            println(io, "myid() = $id")
        end
    catch err
        println(err)
    end
end

ras = [@spawnat w test() for w in workers()]

println("ras: ", ras)

@time wait.(ras)