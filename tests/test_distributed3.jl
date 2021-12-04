using Distributed;

file = "../machines.csv"

machines = readlines(file)

for entry in machines
    local count, host

    v = split(entry, ",")

    host = v[1]

    if length(v) > 1
        count = parse(Int, v[2])
    else
        count = 1
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