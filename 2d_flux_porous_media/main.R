# Eugenio Thieme, eugen@thieme.it, May 2016.

# Simulating the flux of a fluid through a saturated porous media.
# Using finite differences, in the case of quadratic grid and
# Dirichlet boundary conditions.

 
source('functions.R')

n_row <- 10
n_col <- 10

# Initializing a simple model of a confined aquifer
piezometric <- matrix(0, nrow=n_row, ncol=n_col)

# Giving Dirichlet conditions only on two sides (East-West)
piezometric <- set_dirichlet(piezometric, 9, 10)

# Computing piezometric height
piezometric <- get_h(piezometric)
#plot(piezometric)

#image(piezometric)