"""Run mut _build / usgs_1 / mut _post for each listed verification folder."""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path


def find_exe(names: tuple[str, ...]) -> str | None:
    for name in names:
        found = shutil.which(name)
        if found:
            return found
    return None


def find_mut() -> str | None:
    return find_exe(("mut", "mut.exe"))


def find_usgs() -> str | None:
    return find_exe(("usgs_1", "USGS_1", "usgs_1.exe", "USGS_1.exe"))


def _run_cmd(
    cmdline: str,
    cwd: Path,
    log,
    timeout_s: int | None,
) -> tuple[int, str]:
    line = f">>> {cmdline}  (cwd {cwd})"
    log.write(f"\n{line}\n")
    log.flush()
    print(line, flush=True)
    try:
        proc = subprocess.run(
            cmdline,
            cwd=str(cwd),
            shell=True,
            timeout=timeout_s,
        )
    except subprocess.TimeoutExpired:
        msg = f"timed out after {timeout_s}s"
        log.write(msg + "\n")
        print(msg, flush=True)
        return 124, msg
    except OSError as exc:
        msg = f"failed to start: {exc}"
        log.write(msg + "\n")
        print(msg, flush=True)
        return 127, msg
    log.write(f"exit {proc.returncode}\n")
    log.flush()
    print(f"exit {proc.returncode}", flush=True)
    return proc.returncode, ""


def run_one_folder(
    folder_dir: Path,
    mut: str,
    usgs: str,
    log,
    timeout_s: int,
) -> tuple[bool, str]:
    steps = (
        (f'"{mut}" _build', "mut _build"),
        (f'"{usgs}" modflow', "usgs_1 modflow"),
        (f'"{mut}" _post', "mut _post"),
    )
    for cmdline, label in steps:
        code, tail = _run_cmd(cmdline, folder_dir, log, timeout_s)
        if code != 0:
            return False, f"{label} failed (exit {code}): {tail.strip()[:200]}"
    return True, "ok"


def run_batch(
    release_dir: Path,
    folders: list[str],
    timeout_s: int = 3600,
    log_path: Path | None = None,
) -> tuple[list[tuple[str, bool, str]], str]:
    """Run the verification batch. Returns per-folder results and a summary line."""
    log_path = log_path or (release_dir / "_verify_batch.log")
    results: list[tuple[str, bool, str]] = []

    mut = find_mut()
    usgs = find_usgs()
    if mut is None:
        return [], "mut.exe not found on PATH"
    if usgs is None:
        return [], "usgs_1.exe not found on PATH"

    with log_path.open("w", encoding="utf-8") as log:
        log.write(f"release dir: {release_dir}\n")
        log.write(f"mut: {mut}\nusgs: {usgs}\n")
        print(f"batch log: {log_path}", flush=True)
        print("running mut _build / usgs_1 / mut _post per folder", flush=True)
        print(f"mut: {mut}", flush=True)
        print(f"usgs: {usgs}", flush=True)

        n = len(folders)
        for i, folder in enumerate(folders, start=1):
            folder_dir = release_dir / folder
            header = f"======== {i}/{n} {folder} ========"
            log.write(f"\n{header}\n")
            print(header, flush=True)
            if not folder_dir.is_dir():
                results.append((folder, False, "folder missing"))
                log.write("folder missing\n")
                print("folder missing", flush=True)
                continue
            if not (folder_dir / "_build.mut").is_file():
                results.append((folder, False, "no _build.mut"))
                log.write("no _build.mut\n")
                print("no _build.mut", flush=True)
                continue
            ok, msg = run_one_folder(folder_dir, mut, usgs, log, timeout_s)
            results.append((folder, ok, msg))
            print(f"{folder}: {'ok' if ok else 'FAIL'} {msg}", flush=True)

    n_ok = sum(1 for _, ok, _ in results if ok)
    summary = f"{n_ok}/{len(results)} folders succeeded; log {log_path}"
    return results, summary
