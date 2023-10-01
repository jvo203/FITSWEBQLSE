# read all Float64 values from a text file wts.dat
# and store them in a vector

# read all the values from the file
# and store them in a vector
wts = Float64[]

f = open("wts.dat")
while !eof(f)
    push!(wts, parse(Float64, readline(f)))
end

# close the file
close(f)

# print the vector
println(wts)

# read the centres three Float64 values per line
# and store them in an array of arrays
# centres = Array{Float64,2}(undef, 0, 3)
centres = []

f = open("centres.dat")
while !eof(f)
    line = readline(f)
    push!(centres, parse.(Float64, split(line)))
end

# close the file
close(f)

# print the array
println(centres)