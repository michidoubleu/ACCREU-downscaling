simplify_transitions <- function(transitions){

  # Step 1: Combine reverse transitions and calculate net flows
  net_flows <- transitions %>%
    # Create a key for flow between two nodes, ensuring A-B is same as B-A
    mutate(pair = ifelse(lu.from < lu.to, paste(lu.from, lu.to, sep = "-"), paste(lu.to, lu.from, sep = "-"))) %>%
    # Aggregate flows between each pair
    group_by(pair) %>%
    summarise(value = sum(ifelse(lu.from < lu.to, value, -value))) %>%
    ungroup()

  # Step 2: Separate the pairs back inlu.to "lu.from" and "lu.to" nodes
  net_flows <- net_flows %>%
    separate(pair, into = c("lu.from", "lu.to"), sep = "-")

  # Step 3: Make sure all value are positive, and adjust direction if necessary
  net_flows_pos <- net_flows %>% filter(value>=0)

  net_flows_neg <- net_flows %>% filter(value<0) %>% rename(lu.to=lu.from, lu.from=lu.to) %>% mutate(value=abs(value))

  net_flows <- net_flows_pos %>% bind_rows(net_flows_neg)

  # View result
  return(as.data.frame(net_flows))
}