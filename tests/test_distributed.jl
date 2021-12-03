using Distributed;

println(workers())

@everywhere function test()
    id = myid()
    println(id)

    try
        io = open("/data/cache/$(id).txt", "w")
        println(io, "myid() = $id")
        close(io)
    catch err
        println(err)
    end
end

ras = [@spawnat w test() for w in workers()]

println("ras: ", ras)

@time wait.(ras)
