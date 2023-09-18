# Deployment Configuration

## Permissions for log files

The docker configuration uses volumes to link to the file system of the host computer. As a result the status of RAP will survive containers \(i.e. you can start, stop, kill, restart containers without losing data\). However, you must ensure write permission for the container on runtime throughout all three subdirectories of the `volumes` directory. The pod in which RAP runs defines a volume called `log` in which the log files are written and a volume called `scripts` in which user scripts are stored. The pod in which MariaDB runs writes its `mysql`-directory to a volume called `mysql`. This means that the files are stored outside the container on the machine that hosts the docker platform. 

## Docker group

You have a host computer in which the docker platform runs. \(In deploying RAP4 for the OUNL that would be the machine with domain name `rap.cs.ou.nl`\). To avoid permission errors \(and the use of `sudo`\), add your user to the `docker` group. [Read more](https://docs.docker.com/engine/installation/linux/linux-postinstall/).

## Development vs. Production versions

For developing RAP you use slightly different settings. For example you want to reset the database when developing, but this feature must be entirely impossible during production. For this purpose we have two source files: `RAP4dev.adl` and `RAP4prod.adl`. The only difference between the two is configuration settings. Follow the code to find out.

