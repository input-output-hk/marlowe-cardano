{
  # ----------
  # Scheduling
  # ----------
  constraint = [
    {
      attribute = "\${node.class}";
      operator = "=";
      value = "marlowe"; # only run task on marlowe clients, configured in dapps-world
    }
  ];

  spread = [{ attribute = "\${node.datacenter}"; }];
  # ----------
  # Update
  # ----------
  update.health_check = "task_states";
  update.healthy_deadline = "5m0s";
  update.max_parallel = 1;
  update.min_healthy_time = "10s";
  update.progress_deadline = "60m0s";
  update.stagger = "30s";
  # ----------
  # Migrate
  # ----------
  migrate.health_check = "checks";
  migrate.healthy_deadline = "8m20s";
  migrate.max_parallel = 1;
  migrate.min_healthy_time = "10s";
  # ----------
  # Reschedule
  # ----------
  reschedule.delay = "30s";
  reschedule.delay_function = "exponential";
  reschedule.max_delay = "1h0m0s";
  reschedule.unlimited = true;
}
