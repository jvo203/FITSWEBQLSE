using Distributed;

file = "../machines.txt"

machines = readlines(file)

for entry in machines
    local count, host

    v = split(entry, "*")

    if length(v) > 1
        count = parse(Int, v[1])
        host = v[2]
    else
        count = 1
        host = v[1]
    end

    println("host: $host, count: $count")

    if host == "localhost"
        println("adding $count local worker(s)")
        addprocs(count)
    else
        println("adding $count remote worker(s) on $host")
        addprocs([(host, count)])
    end
end

println(workers())