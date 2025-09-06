# add_us_state.R
# function to add USA if the affiliation has a US state
# August 2025

add_us_state = function(states, intext){
  states_end = paste(states, '$', collapse='|', sep='')
  intext = str_replace(intext, states_end, 'United States of America') # just remove state and replace with country
  return(intext)
}