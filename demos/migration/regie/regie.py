#!/usr/bin/env python3
"""The director of the migration demonstrator.

It serves one page with three buttons and a live picture of the state, and it does what the
buttons ask by talking to Docker: it starts and removes the containers of the three systems,
and it tells the "flipper" (a reverse proxy) to which system the traffic of the users goes.
The applications themselves are what `ampersand deploy` generates; nothing in them serves the demo.

The phases follow "Data Migration under a Changing Schema in Ampersand" (RAMiCS 2024):

  existing    users work in the existing system; it is the only system that runs
  preparing   the desired system and the migration system are being started and installed
  prepared    pre-deployment: the migration system follows the existing system
  migrating   after the moment of transition: users work in the migration system
  completed   after the moment of completion: users work in the desired system
"""
import http.cookiejar
import json
import os
import subprocess
import threading
import time
import urllib.error
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

DEMO = os.environ["DEMO"]
APP_PORT = os.environ.get("APP_PORT", "8201")
HERE = os.path.dirname(os.path.abspath(__file__))
SYSTEM = json.load(open("/demo/system.json"))
CONFIG = json.load(open("/demo/demo.json"))
GRANTS = open("/demo/grants.sql").read()

CONTEXT = {c["service"]: c for c in SYSTEM["contexts"]}
# The three systems of a migration, and the service that `ampersand deploy` gave each of them.
SERVICE = {part: CONFIG[part] for part in ("existing", "migration", "desired")}
NETWORK = f"{DEMO}-net"
DB = f"{DEMO}-database"
FLIPPER = f"{DEMO}-flipper"
VOLUME = f"{DEMO}-db-data"
LABEL = f"migration-demo={DEMO}"

lock = threading.Lock()
state = {
    "demo": DEMO,
    "title": CONFIG.get("title", DEMO),
    "story": CONFIG.get("story", {}),
    "appPort": APP_PORT,
    "home": CONFIG.get("home", "/"),   # the screen of the application that the page opens
    "phase": "starting",
    "busy": "starting",       # what the director is doing; "" when it waits for a button
    "route": None,            # the system that serves the users; None while no installed system does
    "ready": False,           # prepared: the migration system has caught up with the existing system
    "violations": [],         # the violations that users still have to repair
    "containers": {},         # part -> "absent" | "starting" | "running"
    "databases": {},          # part -> name of its database, while that database exists
    "error": "",
    "events": [],
}
browser = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar()))  # a user's session
hardening = []      # the conjuncts of the rules whose violations can only disappear
synced = None       # the checksum of the existing database at the last moment the migration system caught up


def container(part):
    return f"{DEMO}-{part}-system"


def database(part):
    return CONTEXT[SERVICE[part]]["defaultDatabase"]


def event(en, nl=None):
    with lock:
        state["events"].append({"time": time.time(), "en": en, "nl": nl or en})
        del state["events"][:-40]
    print(en, flush=True)


def docker(*args, stdin=None, check=True):
    done = subprocess.run(["docker", *args], input=stdin, capture_output=True, text=True)
    if check and done.returncode != 0:
        raise RuntimeError(f"docker {' '.join(args[:3])} …: {done.stderr.strip()[-400:]}")
    return done.stdout


def sql(query, db=None):
    args = ["exec", "-i", DB, "mysql", "-uroot", "-pampersand", "-N", "-B"] + ([db] if db else [])
    return docker(*args, stdin=query).strip()


def call(part, path, timeout=300):
    """A GET on the API of one of the systems, as its administrator would do."""
    with urllib.request.urlopen(f"http://{container(part)}/api/v1/{path}", timeout=timeout) as answer:
        return json.loads(answer.read().decode() or "null")


def wait_until(what, test, seconds=300):
    deadline = time.time() + seconds
    while time.time() < deadline:
        try:
            if test():
                return
        except Exception:
            pass
        time.sleep(1.5)
    raise RuntimeError(f"{what} did not happen within {seconds} seconds")


def answers(part):
    with urllib.request.urlopen(f"http://{container(part)}/", timeout=5) as answer:
        return answer.status == 200


# ---- what the director does to the outside world ------------------------------------------------

def start_database():
    docker("run", "-d", "--name", DB, "--label", LABEL, "--network", NETWORK, "--network-alias", "db",
           "-v", f"{VOLUME}:/var/lib/mysql",
           "-e", "MYSQL_ROOT_PASSWORD=ampersand", "-e", "MYSQL_USER=ampersand", "-e", "MYSQL_PASSWORD=ampersand",
           "mariadb:10.6", "--lower-case-table-names=1", "--sql-mode=ANSI,TRADITIONAL")
    wait_until("the database server", lambda: docker(
        "exec", DB, "mysqladmin", "ping", "-h", "127.0.0.1", "-uroot", "-pampersand", "--silent") is not None, 120)
    sql(GRANTS)


def start_system(part):
    context = CONTEXT[SERVICE[part]]
    names = ";".join(f"{r['label']}={CONTEXT[r['service']]['defaultDatabase']}" for r in context["reaches"])
    args = ["run", "-d", "--name", container(part), "--label", LABEL, "--platform", "linux/amd64",
            "--network", NETWORK, "--network-alias", SERVICE[part],
            "-e", "AMPERSAND_DEBUG_MODE=true", "-e", "AMPERSAND_DBHOST=db",
            "-e", "AMPERSAND_DBUSER=ampersand", "-e", "AMPERSAND_DBPASS=ampersand",
            "-e", f"AMPERSAND_DBNAME={context['defaultDatabase']}",
            "-e", f"AMPERSAND_SERVER_URL=http://localhost:{APP_PORT}"]
    if names:
        args += ["-e", f"AMPERSAND_CONTEXT_DBNAMES={names}"]
    docker(*args, f"{DEMO}-{SERVICE[part]}")


def install(part):
    wait_until(f"the {part} system", lambda: answers(part))
    answer = call(part, "admin/installer")
    if answer.get("errors"):
        raise RuntimeError(f"installing the {part} system: {answer['errors']}")


def flip(part):
    """Send the traffic of the users to one of the three systems."""
    conf = f"""server {{
  listen 80;
  resolver 127.0.0.11 valid=1s;
  client_max_body_size 50m;
  location / {{
    set $system http://{container(part)}:80;
    proxy_pass $system;
    proxy_set_header Host $http_host;
    proxy_read_timeout 300s;
    proxy_hide_header X-Frame-Options;
    proxy_hide_header Content-Security-Policy;
    add_header Cache-Control "no-store" always;
    add_header X-Routed-To "{part}" always;
  }}
}}
"""
    docker("exec", "-i", FLIPPER, "sh", "-c", "cat > /etc/nginx/conf.d/default.conf && nginx -s reload", stdin=conf)
    with lock:
        state["route"] = part


def checksum(part):
    """Changes whenever a user changes the data of a system."""
    db = database(part)
    tables = sql("SELECT table_name FROM information_schema.tables "
                 f"WHERE table_schema = '{db}' AND table_type = 'BASE TABLE' AND table_name NOT LIKE '\\\\_\\\\_%' "
                 "AND table_name NOT LIKE 'prototypecontext%' AND table_name NOT LIKE 'delta\\\\_%' "
                 "AND table_name NOT IN ('session', 'one') ORDER BY 1").split()
    if not tables:
        return ""
    return sql("CHECKSUM TABLE " + ", ".join(f"`{db}`.`{t}`" for t in tables))


def catch_up():
    """Let the migration system take stock of what the existing system holds now."""
    global synced
    before = checksum("existing")
    call("migration", "admin/execengine/run")
    synced = before


def open_violations():
    """The violations of the new rules that the migration system keeps in its database."""
    if not hardening:
        return []
    ids = ", ".join("'" + c.replace("'", "") + "'" for c in hardening)
    rows = sql(f"SELECT src FROM `__conj_violation_cache__` WHERE conjId IN ({ids}) ORDER BY 1", database("migration"))
    atoms = rows.split("\n") if rows else []
    # The picture shows them in the words that the application gives its user.
    try:
        with browser.open(f"http://{container('migration')}/api/v1/app/navbar", timeout=20) as answer:
            signals = json.loads(answer.read().decode())["notifications"]["signals"]
        texts = [v["message"] for s in signals for v in s["violations"]]
        if len(texts) == len(atoms):
            return texts
    except Exception:
        pass
    return atoms


# ---- the buttons --------------------------------------------------------------------------------

def reset():
    """Bring the world to where the demonstration starts: the existing system runs, and nothing else."""
    global hardening, synced
    hardening, synced = [], None
    browser.handlers[:] = [h for h in browser.handlers if not isinstance(h, urllib.request.HTTPCookieProcessor)]
    browser.add_handler(urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar()))
    with lock:
        state.update(phase="starting", route=None, ready=False, violations=[], error="")
    old = docker("ps", "-aq", "--filter", f"label={LABEL}").split()
    if old:
        docker("rm", "-f", *old)
    docker("volume", "rm", "-f", VOLUME, check=False)
    event("The database server starts.", "De databaseserver start.")
    start_database()
    docker("run", "-d", "--name", FLIPPER, "--label", LABEL, "--network", NETWORK, "-p", f"{APP_PORT}:80", "nginx:alpine")
    event("The existing system starts.", "Het bestaande systeem start.")
    start_system("existing")
    wait_until("the flipper", lambda: docker("exec", FLIPPER, "test", "-s", "/var/run/nginx.pid") is not None, 60)
    # The flipper only ever points to a system that is installed: the page loads the application
    # as soon as the route appears, and an application without its database answers every call with 500.
    install("existing")
    flip("existing")
    with lock:
        state["phase"] = "existing"
    event("Users work in the existing system.", "Gebruikers werken in het bestaande systeem.")


def start_migration():
    global hardening
    with lock:
        state["phase"] = "preparing"
    event("The desired system and the migration system start.", "Het gewenste systeem en het migratiesysteem starten.")
    start_system("desired")
    start_system("migration")
    install("desired")
    event("The desired system is installed, with an empty database.", "Het gewenste systeem is geïnstalleerd, met een lege database.")
    install("migration")
    rules = json.loads(docker("exec", container("migration"), "cat", "/var/www/backend/generics/rules.json"))
    hardening = [c for r in rules["signals"] if r.get("hardens") for c in r["conjunctIds"]]
    catch_up()
    with lock:
        state["phase"] = "prepared"
    event("The migration system is installed. It copies what the existing system holds.",
          "Het migratiesysteem is geïnstalleerd. Het kopieert wat het bestaande systeem bevat.")


def switch():
    catch_up()
    flip("migration")
    with lock:
        state["phase"] = "migrating"
    event("The flipper sends the users to the migration system.", "De flipper stuurt de gebruikers naar het migratiesysteem.")


def clean_up():
    if open_violations():
        raise RuntimeError("there are violations left to repair")
    flip("desired")
    call("desired", "admin/ruleengine/evaluate/all")
    event("The flipper sends the users to the desired system.", "De flipper stuurt de gebruikers naar het gewenste systeem.")
    docker("rm", "-f", container("migration"), container("existing"))
    for part in ("migration", "existing"):
        sql(f"DROP DATABASE IF EXISTS `{database(part)}`")
    with lock:
        state["databases"] = {"desired": database("desired")}
        state["phase"] = "completed"
    event("The migration system and the existing system are removed, with their databases.",
          "Het migratiesysteem en het bestaande systeem zijn verwijderd, met hun databases.")


ACTIONS = {
    "start": (start_migration, "existing"),
    "switch": (switch, "prepared"),
    "cleanup": (clean_up, "migrating"),
    "reset": (reset, None),
}


def perform(name):
    action, phase = ACTIONS[name]
    with lock:
        if state["busy"]:
            return "the director is busy"
        if phase and state["phase"] != phase:
            return f"not in phase {phase}"
        if name == "switch" and not state["ready"]:
            return "the migration system has not caught up yet"
        if name == "cleanup" and state["violations"]:
            return "there are violations left to repair"
        state["busy"], state["error"] = name, ""

    def run():
        try:
            action()
        except Exception as problem:
            with lock:
                state["error"] = str(problem)
            event(f"Problem: {problem}", f"Probleem: {problem}")
        finally:
            with lock:
                state["busy"] = ""

    threading.Thread(target=run, daemon=True).start()
    return ""


# ---- watching the world -------------------------------------------------------------------------

def watch():
    while True:
        try:
            running = {}
            for line in docker("ps", "-a", "--filter", f"label={LABEL}", "--format", "{{.Names}}\t{{.State}}").splitlines():
                name, status = line.split("\t")
                running[name] = status
            containers = {}
            for part in SERVICE:
                status = running.get(container(part))
                containers[part] = "absent" if status is None else "running" if status == "running" else "starting"
            containers["database"] = "running" if running.get(DB) == "running" else "absent"
            containers["flipper"] = "running" if running.get(FLIPPER) == "running" else "absent"
            existing = sql("SHOW DATABASES").split() if containers["database"] == "running" else []
            databases = {part: database(part) for part in SERVICE if database(part) in existing}
            with lock:
                phase, busy = state["phase"], state["busy"]
            ready, violations = False, []
            if phase == "prepared" and not busy:
                if checksum("existing") != synced:
                    catch_up()
                ready = checksum("existing") == synced
            if phase in ("prepared", "migrating"):
                violations = open_violations()
            with lock:
                state.update(containers=containers, databases=databases, violations=violations)
                state["ready"] = ready and state["phase"] == "prepared"
        except Exception as problem:
            print(f"watch: {problem}", flush=True)
        time.sleep(1)


class Page(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def send(self, status, kind, body):
        data = body if isinstance(body, bytes) else body.encode()
        self.send_response(status)
        self.send_header("Content-Type", kind)
        self.send_header("Content-Length", str(len(data)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        if self.path.split("?")[0] == "/":
            page = open(os.path.join(HERE, "page.html"), encoding="utf-8").read()
            # The language of the page follows the example, unless the address asks for the other one.
            self.send(200, "text/html; charset=utf-8", page.replace("__LANG__", "en" if CONFIG.get("lang") == "en" else "nl"))
        elif self.path == "/state":
            with lock:
                self.send(200, "application/json", json.dumps(state))
        else:
            self.send(404, "text/plain", "not found")

    def do_POST(self):
        name = self.path.rsplit("/", 1)[-1]
        if not self.path.startswith("/action/") or name not in ACTIONS:
            return self.send(404, "text/plain", "not found")
        refusal = perform(name)
        self.send(409 if refusal else 202, "application/json", json.dumps({"refused": refusal}))


if __name__ == "__main__":
    threading.Thread(target=watch, daemon=True).start()
    with lock:
        state["busy"] = "reset"

    def begin():
        try:
            reset()
        except Exception as problem:
            with lock:
                state["error"] = str(problem)
            event(f"Problem: {problem}", f"Probleem: {problem}")
        finally:
            with lock:
                state["busy"] = ""

    threading.Thread(target=begin, daemon=True).start()
    ThreadingHTTPServer(("0.0.0.0", 8000), Page).serve_forever()
