using Genie, Genie.Router, Genie.Renderer, Genie.Requests
using HTTP
using Distributed

println("Started julia with $(nworkers()) threads.")

Genie.config.run_as_server = true

route("/") do
    # (:result => "Hello") |> json
    "Hello Genie"
end

Genie.startup()