# tricky_funder_match.R
# tricky match used for funders with similar names
# Feb 2026

tricky_match = function(intext, funder_data, funder_nums){
  nums_out = NULL
  # get long names for second funder (assuming funder numbers are in order with potential false positive first)
  lower = filter(funder_data, funder_number == funder_nums[2]) %>% pull(lower_case) # get all funder names for number 2
  match_two = str_detect(intext, pattern = lower)
  if(match_two == TRUE){ 
    intext = str_remove_all(intext, pattern = lower) # remove all matches from text
    nums_out = c(nums_out, funder_nums[2]) # add funder 2 to matches
  }
  # get long names for first funder
  lower = filter(funder_data, funder_number == funder_nums[1]) %>% pull(lower_case) # get all funder names for number 1
  match_one = str_detect(intext, pattern = lower) # search in remaining text
  if(match_one == TRUE){ # 
    nums_out = c(nums_out, funder_nums[1]) # add funder 1 to matches
  }
  # return
  return(nums_out)
}
