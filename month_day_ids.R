# month_day_ids()

# make a list of ids (1 through 12), repeated by the number of days in each month
# useful to converting daily to monthly values using apply functions (e.g., StackApply)

month_day_ids = function(leap  # T or F.  T for leap year, F for non-leap year
                         ){
  if(leap == T){
    # leap year
    month.data.leap = read.csv("/net/home/eos/dgrogan/git_repos/WBMr/data/days_in_months_leap.csv")
    m.ids = unlist(mapply(FUN = function(x,y)rep(x,y), x = seq(1,12), y = month.data.leap$days))
  }else{
    # non-leap year
    month.data = read.csv("/net/home/eos/dgrogan/git_repos/WBMr/data/days_in_months.csv")
    m.ids = unlist(mapply(FUN = function(x,y)rep(x,y), x = seq(1,12), y = month.data$days))
  }
  m.ids
}

