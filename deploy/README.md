# Developer Service Deployment
## Access
The development server is accessible through a ssh daemon running as a task in the same namespace and network as the marlowe-runtime services for each environment.

Access is granted through your GitHub user's ssh key and you can login like so:

```
ssh dev@dapps.aws.iohkdev.io -p 4022
```

## Network
Information about the IP address and ports of each service is located in /local/network.env of the ssh daemon's container.

Running `scripts/setup-dev-connection` will query that file and setup ssh port forwarding for chainseekd and all marlowe services.

Ssh port forwarding will let you connect to ports bound on your local machine and have the stream forwarded to the service running on the remote machine. It will be as if you have all the services running on your local machine.

## Configuration
### Operables and OCI Images
Each service has an operable in `deploy/operables.nix`. These are written to be scheduler-agnostic and get configured through environment variables which are documented at the top of each operable.

Each operable has a related OCI image containing exactly the operable and its debug and runtime dependencies.
### Nomad Tasks and Environment
Nomad tasks for each service are in `deploy/nomadTasks.nix`. These are meant to be put together in a Nomad environment. They can be configured with Nomad's meta stanza.

The Nomad environment in `deploy/nomadEnv.nix` puts the tasks together, configures ports and services for each task, and creates tasks for each Cardano environment.

## Management
### Setting up Ops devshell
To manage services or access the postgres shell you will need to setup your github credentials for the ops devshell to authenticate you.

Create a personal access token on github.com with the entire `repo` and `workflow` scope: Settings -> Developer Settings -> Personal access tokens -> Tokens (classic) -> Generate new token (classic).

Copy the token from github and then open your ~/.netrc and add github credentials in the following format:

```

machine github.com login <github username> password <token>
machine api.github.com login <github username> password <token>
```

Then you can enter the `ops` devshell in this repo: `nix develop .#ops`.

If it worked right you should be able to see the `NOMAD_TOKEN` with `env | grep NOMAD_TOKEN`.
### Nomad Web UI
To login to https://nomad.dapps.aws.iohkdev.io, run `nomad ui -authenticate`. You can also enter the token manually into that webiste if the command doesn't work. Make sure to login with your IOHK google account.

In this webiste, you can view logs and restart/manage individual jobs and tasks.

### Access shell for database or other tasks
To access the postgres shell with the DB sync and chain-indexer tables, you can use nomad-exec once your in the ops devshell.

Run `nomad-exec -s -n infra`, choose `database` then `patroni`. Look for the allocation with `infra-database:master` and choose that one.

Once your in the Debug shell run, `psql -h /alloc -U dba -d postgres`. All tables will be accessible from this user.

To access the shell of any marlowe service run `nomad-exec -s -n marlowe` and select the job and task.
