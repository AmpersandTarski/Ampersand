---
description: >-
  This page is under construction and does not work yet. Help is appreciated.
  Currently, the move from docker-compose to Kubernetes has low priority.
---

# Deploying with Kubernetes

## Purpose

Deploying your Ampersand application to production can be done fast and frequent. For this purpose we are working on deployment on the Kubernetes platform. This allows you to deploy easily on your laptop \(for testing\), in your own data center \(The minimum you need is a virtual machiine that runs kubernetes\), or in the cloud \(to benefit from the reliability and security provided by hosting providers\). We use Kubernetes because it is open, free, well known, widely used, and is continuously being impoved by a large community. It lets you deploy fast and lets you upgrade the application without taking it offline.

## Way of working

1. A Kubernetes cluster is needed as the platform from which to launch and maintain the application. If it is there, that's fine. Otherwise you get one from a provider, from a data center, or you create one yourself \(e.g. on your laptop\). The cluster is built on top of virtual or physical machines and hosts and exposes the services that constitute the application.
2. On the platform I need a registry from which Kubernetes can pull the image\(s\).

I tried it out for myself. I stole my inspiration from [Kevin Smets' instruction](https://gist.github.com/kevin-smets/b91a34cea662d0c523968472a81788f7). Thank you Kevin! I ran the following from my terminal \(i.e. MacOS cli\), just for the purpose of getting some hands-on experience...

### Requirements

To make a Kubernetes cluster, I used Minikube. Minikube requires that VT-x/AMD-v virtualization is enabled in BIOS. To check that this is enabled on OSX / macOS, I ran:

```text
sysctl -a | grep machdep.cpu.features | grep VMX
```

If there's output, it works!

### Prerequisites

* kubectl
* docker \(for Mac\)
* minikube
* virtualbox

```text
brew update && brew install kubectl && brew cask install docker minikube virtualbox
```

### Verify

```text
docker --version                # Docker version 17.09.0-ce, build afdb6d4
docker-compose --version        # docker-compose version 1.16.1, build 6d1ac21
docker-machine --version        # docker-machine version 0.12.2, build 9371605
minikube version                # minikube version: v0.22.3
kubectl version --client        # Client Version: version.Info{Major:"1", Minor:"8", GitVersion:"v1.8.1", GitCommit:"f38e43b221d08850172a9a4ea785a86a3ffa3b3a", GitTreeState:"clean", BuildDate:"2017-10-12T00:45:05Z", GoVersion:"go1.9.1", Compiler:"gc", Platform:"darwin/amd64"}      
```

### Start

```text
minikube start
```

This can take a while, expected output:

```text
Starting local Kubernetes cluster...
Kubectl is now configured to use the cluster.
```

Great! You now have a running Kubernetes cluster locally. Minikube started a virtual machine for you, and a Kubernetes cluster is now running in that VM.

### Check k8s

```text
kubectl get nodes
```

Should output something like:

```text
NAME       STATUS    ROLES     AGE       VERSION
minikube   Ready     <none>    40s       v1.7.5
```

Within minikube, a docker platform is running. However, on my Mac I have another docker daemon running. So which docker do we want to talk to? In this case that is definitely the docker daemon that is inside minikube...

### Use minikube's built-in docker daemon

```text
eval $(minikube docker-env)
```

\(You might add this line to `.bash_profile` or `.zshrc` or ...  to use minikube's daemon by default. Or if you do not want to set this every time you open a new terminal\).

If I want to talk to the docker daemon on my Mac, I have to revert back to it by running:

```text
eval $(docker-machine env -u)
```

When running `docker ps`, it showed the following output:

```text
CONTAINER ID        IMAGE                                      COMMAND                  CREATED             STATUS              PORTS               NAMES
3835176d93bb        k8s.gcr.io/k8s-dns-sidecar-amd64           "/sidecar --v=2 --lo…"   30 minutes ago      Up 30 minutes                           k8s_sidecar_kube-dns-86f4d74b45-4b48w_kube-system_91346644-e424-11e8-bae2-080027501972_0
3c803544be8a        k8s.gcr.io/kubernetes-dashboard-amd64      "/dashboard --insecu…"   30 minutes ago      Up 30 minutes                           k8s_kubernetes-dashboard_kubernetes-dashboard-6f4cfc5d87-mvtgw_kube-system_962829f4-e424-11e8-bae2-080027501972_0
1d74c735c1df        k8s.gcr.io/coredns                         "/coredns -conf /etc…"   30 minutes ago      Up 30 minutes                           k8s_coredns_coredns-c4cffd6dc-xn62h_kube-system_961c845a-e424-11e8-bae2-080027501972_0
308bd132b35a        gcr.io/k8s-minikube/storage-provisioner    "/storage-provisioner"   31 minutes ago      Up 30 minutes                           k8s_storage-provisioner_storage-provisioner_kube-system_963bf651-e424-11e8-bae2-080027501972_0
```

The list was in fact a little bit longer: it contained 22 containers.

#### Build, deploy and run an image on your local k8s setup

To setup a local registry, so Kubernetes can pull the image\(s\) from there:

```text
docker run -d -p 5000:5000 --restart=always --name registry registry:2
```

To test whether there is a local registry, ask docker:

```bash
BA92-C02VP224HTDF:RAP stefjoosten$ docker ps | grep registry
97dc5ed7102d        registry:2                                 "/entrypoint.sh /etc…"   4 days ago          Up 4 days           0.0.0.0:5000->5000/tcp   registry
BA92-C02VP224HTDF:RAP stefjoosten$ 

```

### Build

First of, store all files \(Dockerfile, my-app.yml, index.html\) in this gist locally in some new \(empty\) directory.

You can build the Dockerfile below locally if you want to follow this guide to the letter. Store the Dockerfile locally, preferably in an empty directory and run:

```text
docker build . --tag my-app
```

You should now have an image named 'my-app' locally, check by using `docker images` \(or your own image of course\). You can then publish it to your local docker registry:

```text
docker tag my-app localhost:5000/my-app:0.1.0
```

Running `docker images` should now output the following:

```text
REPOSITORY                                             TAG                 IMAGE ID            CREATED             SIZE
my-app                                                 latest              cc949ad8c8d3        44 seconds ago      89.3MB
localhost:5000/my-app                                  0.1.0               cc949ad8c8d3        44 seconds ago      89.3MB
httpd                                                  2.4-alpine          fe26194c0b94        7 days ago          89.3MB
```

### Deploy and run from docker-compose.yml

I have a file called `docker-compose.yml`, which I have used to deploy RAP with docker-compose \(rather than Kubernetes\). I used `kompose` to transform that into the necessary Kubernetes .yaml files.

```text
brew install kompose
```

I deployed straight from the `docker-compose.yml` file:

```text
kompose up
```

However, I can also create the necessary Kubernetes files and deploy from `kubectl`:

```text
kompose convert
```

This converts `docker-compose.yml` into the following files:

```text
db-claim0-persistentvolumeclaim.yaml
db-deployment.yaml
phptools-deployment.yaml
phptools-service.yaml
rap3-claim0-persistentvolumeclaim.yaml
rap3-claim1-persistentvolumeclaim.yaml
rap3-deployment.yaml
rap3-service.yaml
```

Having run `kompose up` to deploy the three RAP services, I now have the following situation on my minikube:

```text
BA92-C02VP224HTDF:RAP stefjoosten$ kubectl get all
NAME                            READY   STATUS    RESTARTS   AGE
pod/db-56c6b7b6cc-442k9         1/1     Running   1          3d
pod/phptools-555cc5db49-w7z6k   1/1     Running   0          3d
pod/rap3-668d94bb95-cp8vs       1/1     Running   0          3d

NAME                 TYPE        CLUSTER-IP       EXTERNAL-IP   PORT(S)    AGE
service/kubernetes   ClusterIP   10.96.0.1        <none>        443/TCP    4d
service/phptools     ClusterIP   10.107.212.219   <none>        8080/TCP   3d
service/rap3         ClusterIP   10.101.81.211    <none>        80/TCP     3d

NAME                       DESIRED   CURRENT   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/db         1         1         1            1           3d
deployment.apps/phptools   1         1         1            1           3d
deployment.apps/rap3       1         1         1            1           3d

NAME                                  DESIRED   CURRENT   READY   AGE
replicaset.apps/db-56c6b7b6cc         1         1         1       3d
replicaset.apps/phptools-555cc5db49   1         1         1       3d
replicaset.apps/rap3-668d94bb95       1         1         1       3d
```

\(I got even more information with the command `kubectl get all --output=wide`.\)

So we see three pods \(containers\) running. Each of the pods is wrapped in a service of its own and exposed internally on the local network of this cluster. \(In Kubernetes, [nodes](https://kubernetes.io/docs/admin/node), [pods](https://kubernetes.io/docs/user-guide/pods) and [services](https://kubernetes.io/docs/user-guide/services) all have their own IPs. In many cases, the node IPs, pod IPs, and some service IPs on a cluster will not be routable, so they will not be reachable from a machine outside the cluster, such as your desktop machine.\)

To access these services from a browser, we now  need to expose these services to localhost. I did this by exposing both services from the minikube platform:

```text
BA92-C02VP224HTDF:RAP stefjoosten$ minikube service rap3
Opening kubernetes service default/rap3 in default browser...
BA92-C02VP224HTDF:RAP stefjoosten$ minikube service phptools
Opening kubernetes service default/phptools in default browser...
```

### Deploy and run from Kubernetes yml files

Store the file below `my-app.yml` on your system and run the following:

```text
kubectl create -f my-app.yml
```

You should now see your pod and your service:

```text
kubectl get all
```

The configuration exposes `my-app` outside of the cluster, you can get the address to access it by running:

```text
minikube service my-app --url
```

This should give an output like `http://192.168.99.100:30304` \(the port will most likely differ\). Go there with your favorite browser, you should see "Hello world!". You just accessed your application from outside of your local Kubernetes cluster!

### Kubernetes GUI

```text
minikube dashboard
```

### Delete deployment of my-app

```text
kubectl delete deploy my-app
kubectl delete service my-app
```

You're now good to go and deploy other images!

## Reset everything

```text
minikube stop;
minikube delete;
rm -rf ~/.minikube .kube;
brew uninstall kubectl;
brew cask uninstall docker virtualbox minikube;
```

### Version

Last tested on 2017 October 20th macOS Sierra 10.12.6

