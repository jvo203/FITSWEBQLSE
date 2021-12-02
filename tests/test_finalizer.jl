mutable struct FITSDataSet
    # metadata
    datasetid::String
    x::Float64

    function FITSDataSet()
        new("DUMMY", -1.0)
    end

    function FITSDataSet(datasetid)
        new(datasetid, 7.0)
    end
end

a = FITSDataSet()
b = FITSDataSet("ALMA")

finale(x) = @async println("Finalizing $(x.datasetid).")
finalizer(finale, a)

#finalizer(a) do x
#    @async println("Finalizing $x.")
#end

println(a)
println(b)

a = b
println(a)