# Define the run function
run <- function(){
  visualenrollmentApp(options=list(host='0.0.0.0', port = 8080))
}

# Load package and run the app
devtools::load_all(); run()
