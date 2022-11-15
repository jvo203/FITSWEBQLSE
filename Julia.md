# speed up start-up (just a little bit)

julia --startup-file=no -e 'using DaemonMode; serve()'&

# Julia dependencies (start julia, press ']' to get into a package manager,
then use 'add' to install all the packages):

DaemonMode ArgParse CodecBzip2 CodecLz4 ConfParser Downloads HTTP JSON LibPQ Tables SQLite WebSockets x265_jll ZfpCompression DistributedArrays CFITSIO Images ImageTransformations Interpolations PhysicalConstants ThreadsX WCS LogRoller

# running Julia on jvof or py1

set "JULIA_NUM_THREADS" in .bashrc so that all the distributed workers can use threads
export JULIA_NUM_THREADS=8

# no distributed workers, only threads (not recommended)
julia -O3 Julia/fitswebqlse.jl

# threads with distributed workers for (better CPU load balancing)
julia -O3 -p 4 Julia/fitswebqlse.jl

# testing Julia

julia -p 2 -O3 Julia/fitswebqlse.jl

# running Julia in a cluster

julia -O3 --machine-file machines.txt Julia/fitswebqlse.jl

# a JVO cluster with a persistent worker id order

# AMD
julia -O3 Julia/fitswebqlse.jl --machines machines.dat

# Intel (SSD)
julia -O3 Julia/fitswebqlse.jl --machines machines_intel.dat --config config_ssd.ini

# Redirect of stdout/stderr to a log file:
julia -O3 -p 4 Julia/fitswebqlse.jl > LOGS/out.log 2>&1

# an example config.ini

[fitswebql]
port=9000 ; optional
local=false ; optional
production=false ; optional (true --> 'wss://', false --> 'ws://')
timeout=15 ; [s] , optional (setting 0 disables a timeout)
home=.cache ; optional, i.e. /mnt/fits/files
cache=.cache ; optional, can include two caches: /ssd/cache:/data/cache
logs=LOGS ; optional

[postgresql]
user=jvo
password=??? ; optional
host=p10.vo.nao.ac.jp
port=5433 ; optional
home=/home

[zeromq]
port=50000