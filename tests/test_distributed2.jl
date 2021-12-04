using Distributed;

# add two local workers
addprocs(2)

# add TCP/IP workers (two per node)
addprocs([("grid61", 2)])
addprocs([("grid62", 2)])
addprocs([("grid63", 2)])

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