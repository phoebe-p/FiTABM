# To run any simulations, you need to do two things:
# 1. Load the relevant data using load_data() or load_data_f()
# 2. Run the simulation using batch_run_func() or batch_run_func_f()
# The "_f" means that these functions are used for projections (2016-2022),
# while the other functions are for historical simulations 2010-2016.


## Realistic historical

# Say we want to run the realistic historical scenario (e.g. the one that 
# is designed to mimic what actually happened in the Great Britain 2010-2016.)
# This is the default for the data loading function:

load_data()

# To run the simulation:

batch_run_func(number_of_agents = 500)

# The default number of agents is 5000, and the default number of runs is 10. So just running batch_run_func() does
# 10 runs with 5000 agents. I've put number_of_agents = 500 here to speed things up.

# batch_run_func will automatically plot data and output some key results. If you want to save your data:

batch_run_func(save_name = "test")


## Realistic future

# This is a projection based on UK policy as announced, again, this is the default. 
# However, we must first generate suitable agent populations. This is done using the function
# generate_populations_f():

generate_populations_f(n_agents = 500, n_pop = 5, dev = 200)

# This generates 5 populations of 500 agents, which deviate less than 200 MW from the capacity as it was in
# October 2016 (this isn't very good - but generating 10 populations of 5000 agents which deviate < 25 MW
# is extremely time-consuming!)


# Then you can run projections the same way as historical simulations:

load_data_f()

batch_run_func_f()

# In practice, you only need to do the time-consuming part (generating the agent populations) once,
# then use them to run whatever scenarios you're interested in.