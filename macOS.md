# 0. Homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 1. gcc + gfortran
    brew install gfortran

# 2. memory model
    -mcmodel=medium --> large
    Oh no, -mcmodel=large results in "error: invalid variant 'BLEAH'" on Apple Silicon

# 3. ISPC
    brew install ispc