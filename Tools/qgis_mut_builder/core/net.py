"""HTTPS set-up shared by ``requests`` and GDAL ``/vsicurl/`` reads.

QGIS's bundled OpenSSL does not read the Windows certificate store, so sites
re-signed by antivirus or corporate proxies fail verification. A combined CA
bundle (certifi + Windows ROOT/CA stores) is written once per session and
handed to both ``requests`` and GDAL.
"""

from __future__ import annotations

import os
import ssl
import sys
import tempfile
from pathlib import Path

_BUNDLE: Path | None = None
_SESSION = None

USER_AGENT = "MUT-Model-Builder/0.1 (+https://qgis.org)"


def _cache_dir() -> Path:
    base = os.environ.get("LOCALAPPDATA") or tempfile.gettempdir()
    path = Path(base) / "qgis_mut_builder"
    path.mkdir(parents=True, exist_ok=True)
    return path


def ca_bundle() -> Path:
    """Return a PEM file holding certifi roots plus the Windows ROOT/CA stores."""
    global _BUNDLE
    if _BUNDLE is not None and _BUNDLE.exists():
        return _BUNDLE

    pems: list[str] = []
    try:
        import certifi

        pems.append(Path(certifi.where()).read_text(encoding="ascii", errors="ignore"))
    except ImportError:
        pass
    if sys.platform == "win32":
        for store in ("ROOT", "CA"):
            try:
                for der, encoding, _trust in ssl.enum_certificates(store):
                    if encoding == "x509_asn":
                        pems.append(ssl.DER_cert_to_PEM_cert(der))
            except OSError:
                continue

    bundle = _cache_dir() / "ca_bundle.pem"
    bundle.write_text("\n".join(pems), encoding="ascii")
    _BUNDLE = bundle
    return bundle


def prepare_network() -> Path:
    """Configure process-wide HTTPS settings for GDAL and return the CA bundle path.

    ``SSLKEYLOGFILE`` set by network-monitor filters makes QGIS's OpenSSL abort
    with ``OPENSSL_Uplink ... no OPENSSL_Applink``, so it is removed for this
    process only.
    """
    os.environ.pop("SSLKEYLOGFILE", None)
    bundle = ca_bundle()
    # requests prefers these environment variables over Session.verify.
    os.environ["REQUESTS_CA_BUNDLE"] = str(bundle)
    os.environ["CURL_CA_BUNDLE"] = str(bundle)

    from osgeo import gdal

    gdal.UseExceptions()
    gdal.SetConfigOption("CURL_CA_BUNDLE", str(bundle))
    gdal.SetConfigOption("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
    gdal.SetConfigOption("GDAL_HTTP_MAX_RETRY", "3")
    gdal.SetConfigOption("GDAL_HTTP_RETRY_DELAY", "2")
    gdal.SetConfigOption("GDAL_HTTP_USERAGENT", USER_AGENT)
    gdal.SetConfigOption("VSI_CACHE", "TRUE")
    gdal.SetConfigOption("GDAL_CACHEMAX", "512")
    proxy = os.environ.get("HTTPS_PROXY") or os.environ.get("https_proxy")
    if proxy:
        gdal.SetConfigOption("GDAL_HTTP_PROXY", proxy)
    return bundle


def session():
    """Return a shared ``requests.Session`` that trusts the combined CA bundle."""
    global _SESSION
    if _SESSION is None:
        import requests

        bundle = prepare_network()
        _SESSION = requests.Session()
        _SESSION.verify = str(bundle)
        _SESSION.headers["User-Agent"] = USER_AGENT
    return _SESSION


def download(url: str, dest: Path, params=None, timeout: int = 300, chunk: int = 1 << 20, is_canceled=None) -> Path:
    """Stream ``url`` to ``dest`` (written via a ``.part`` file) and return ``dest``.

    ``is_canceled`` is checked between chunks; a canceled download removes the ``.part`` file.
    """
    dest.parent.mkdir(parents=True, exist_ok=True)
    part = dest.with_suffix(dest.suffix + ".part")
    with session().get(url, params=params, stream=True, timeout=timeout) as resp:
        resp.raise_for_status()
        ctype = resp.headers.get("content-type", "")
        if "xml" in ctype or "json" in ctype or "html" in ctype:
            raise RuntimeError(f"Server returned {ctype} instead of a raster: {resp.text[:300]}")
        try:
            with open(part, "wb") as handle:
                for block in resp.iter_content(chunk):
                    if is_canceled is not None and is_canceled():
                        raise InterruptedError("Run canceled")
                    handle.write(block)
        except BaseException:
            part.unlink(missing_ok=True)
            raise
    part.replace(dest)
    return dest
