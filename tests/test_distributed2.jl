using Distributed;

# add two local workers
addprocs(1)
addprocs(1)

# add TCP/IP workers (2 per node)
addprocs(["grid61"])
addprocs(["grid61"])

addprocs(["grid62"])
addprocs(["grid62"])

addprocs(["grid63"])
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