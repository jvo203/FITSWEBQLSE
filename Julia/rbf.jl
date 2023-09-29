# read all Float64 values from a text file wts.dat
# and store them in a vector

# open the file
f = open("wts.dat")

# read all the values from the file
# and store them in a vector
wts = Float64[]
while !eof(f)
    push!(wts, parse(Float64, readline(f)))
end

# close the file
close(f)

# print the vector
println(wts)
