
create_frame_plan <- function(gage_years, gage_melt, width, height){
  step1 <- create_task_step(
    step_name = 'plot',
    target_name = function(task_name, step_name, ...) {
      sprintf('twitter/gage_age_%s.png', task_name)
    },
    command = function(task_name, step_name, ...) {
      sprintf('plot_sites(target_name, gage_melt, state_map = state_map, yr = %s, width = %s, height = %s)', task_name, width, height)
    }
  )
  task_plan <- create_task_plan(as.character(gage_years), list(step1),
                                final_steps='plot', add_complete = FALSE)
  return(task_plan)
}


tibble_hash_frames <- function(...){
  tibble(filename = c(...), hash = tools::md5sum(filename))
}

create_gif_frames <- function(gage_years, gage_melt, states, sites_file, width, height, ...){
  frame_plan <- create_frame_plan(gage_years, gage_melt, width = width, height = height)
  
  frame_makefile <- "frame_tasks.yml"
  
  create_task_makefile(
    task_plan = frame_plan, 
    makefile = frame_makefile,
    sources = c(...),
    include = "remake.yml",
    final_targets = 'hash_table',
    finalize_funs = 'tibble_hash_frames',
    as_promises = FALSE)
  
  hash_table <- scmake('hash_table',  remake_file = "frame_tasks.yml")
  return(hash_table)

}
