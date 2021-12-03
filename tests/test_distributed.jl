using Distributed;

println(workers())

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
