# Developer Service Deployment
## Access
The development server is accessible through a ssh daemon running as a task in the same namespace and network as the marlowe-runtime services for each environment.

Access is granted through your GitHub user's ssh key and you can login like so:

```
ssh dev@dapps.aws.iohkdev.io -p 4022
```

## Network
Information about the IP address and ports of each service is located in /local/network.env of the ssh daemon's container.

Running `scripts/setup-dev-connection` will query that file and setup ssh port forwarding for marlowe-chain-sync and all marlowe services.

Ssh port forwarding will let you connect to ports bound on your local machine and have the stream forwarded to the service running on the remote machine. It will be as if you have all the services running on your local machine.

## Configuration
### Operables and OCI Images
Each service has an operable in `./operables.nix`. These are written to be scheduler-agnostic and get configured through environment variables which are documented at the top of each operable.

Each operable has a related OCI image containing exactly the operable and its debug and runtime dependencies.

To push an oci-image to the registry you will need to get access credentials, by running `skopeo login` or `docker login` in a devshell with skopeo or docker (ask for login information).
Then to push, for example, marlowe-chain-sync run:

``` sh
nix run .\#oci-images.x86_64-linux.marlowe-chain-sync.copyToRegistry
```

`
### Nomad Tasks and Environment
Nomad tasks for each service are in `./nomadTasks.nix`. These are meant to be put together in a Nomad environment. They can be configured with Nomad's meta stanza.

The Nomad environment in `./nomadEnv.nix` puts the tasks together, configures ports and services for each task, and creates tasks for each Cardano environment.
