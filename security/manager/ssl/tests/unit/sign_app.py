#!/usr/bin/env python
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

"""
Given a directory of files, packages them up and signs the
resulting zip file. Mainly for creating test inputs to the
nsIX509CertDB.openSignedAppFileAsync API.
"""
from base64 import b64encode
from cbor2 import dumps
from cbor2.types import CBORTag
from hashlib import sha1, sha256
import StringIO
import argparse
import os
import pycert
import pycms
import pykey
import re
import zipfile


ES256 = -7
ES384 = -35
ES512 = -36
KID = 4
ALG = 1
COSE_Sign = 98

def coseAlgorithmToPykeyHash(algorithm):
    """Helper function that takes one of (ES256, ES384, ES512)
    and returns the corresponding pykey.HASH_* identifier."""
    if algorithm == ES256:
        return pykey.HASH_SHA256
    elif algorithm == ES384:
        return pykey.HASH_SHA384
    elif algorithm == ES512:
        return pykey.HASH_SHA512
    else:
        raise UnknownCOSEAlgorithmError(algorithm)

# COSE_Signature = [
#     protected : serialized_map,
#     unprotected : {},
#     signature : bstr
# ]
def coseSignature(payload, algorithm, signingKey, signingCertificate,
                  bodyProtected):
    """Returns a COSE_Signature structure.
    payload is a string representing the data to be signed
    algorithm is one of (ES256, ES384, ES512)
    signingKey is a pykey.ECKey to sign the data with
    signingCertificate is a byte string
    bodyProtected is the serialized byte string of the protected body header
    """
    protected = {ALG: algorithm, KID: signingCertificate}
    protectedEncoded = dumps(protected)
    # Sig_structure = [
    #     context : "Signature"
    #     body_protected : bodyProtected
    #     sign_protected : protectedEncoded
    #     external_aad : nil
    #     payload : bstr
    # ]
    sigStructure = [u'Signature', bodyProtected, protectedEncoded, None,
                    payload]
    sigStructureEncoded = dumps(sigStructure)
    pykeyHash = coseAlgorithmToPykeyHash(algorithm)
    signature = signingKey.signRaw(sigStructureEncoded, pykeyHash)
    return [protectedEncoded, {}, signature]

# COSE_Sign = [
#     protected : serialized_map,
#     unprotected : {},
#     payload : nil,
#     signatures : [+ COSE_Signature]
# ]
def coseSig(payload, intermediates, signatures):
    """Returns the entire (tagged) COSE_Sign structure.
    payload is a string representing the data to be signed
    intermediates is an array of byte strings
    signatures is an array of (algorithm, signingKey,
               signingCertificate) triplets to be passed to
               coseSignature
    """
    protected = {KID: intermediates}
    protectedEncoded = dumps(protected)
    coseSignatures = []
    for (algorithm, signingKey, signingCertificate) in signatures:
        coseSignatures.append(coseSignature(payload, algorithm, signingKey,
                                            signingCertificate,
                                            protectedEncoded))
    tagged = CBORTag(COSE_Sign, [protectedEncoded, {}, None, coseSignatures])
    return dumps(tagged)

def walkDirectory(directory):
    """Given a relative path to a directory, enumerates the
    files in the tree rooted at that location. Returns a list
    of pairs of paths to those files. The first in each pair
    is the full path to the file. The second in each pair is
    the path to the file relative to the directory itself."""
    paths = []
    for path, dirs, files in os.walk(directory):
        for f in files:
            fullPath = os.path.join(path, f)
            internalPath = re.sub(r'^/', '', fullPath.replace(directory, ''))
            paths.append((fullPath, internalPath))
    return paths

def addManifestEntry(filename, hashes, contents, entries):
    """Helper function to fill out a manifest entry.
    Takes the filename, a list of (hash function, hash function name)
    pairs to use, the contents of the file, and the current list
    of manifest entries."""
    entry = 'Name: %s\n' % filename
    for (hashFunc, name) in hashes:
        base64hash = b64encode(hashFunc(contents).digest())
        entry += '%s-Digest: %s\n' % (name, base64hash)
    entries.append(entry)

def coseAlgorithmToSignatureParams(coseAlgorithm, issuerName):
    """Given a COSE algorithm ('ES256', 'ES384', 'ES512') and an issuer
    name, returns a (algorithm id, pykey.ECCKey, encoded certificate)
    triplet for use with coseSig.
    """
    if coseAlgorithm == 'ES256':
        keyName = 'secp256r1'
        algId = ES256
    elif coseAlgorithm == 'ES384':
        keyName = 'secp384r1'
        algId = ES384
    elif coseAlgorithm == 'ES512':
        keyName = 'secp521r1' # COSE uses the hash algorithm; this is the curve
        algId = ES512
    else:
        raise UnknownCOSEAlgorithmError(coseAlgorithm)
    key = pykey.ECCKey(keyName)
    certSpecification = 'issuer:%s\n' % issuerName + \
        'subject: xpcshell signed app test signer\n' + \
        'subjectKey:%s\n' % keyName + \
        'extension:keyUsage:digitalSignature'
    certSpecificationStream = StringIO.StringIO()
    print >>certSpecificationStream, certSpecification
    certSpecificationStream.seek(0)
    cert = pycert.Certificate(certSpecificationStream)
    return (algId, key, cert.toDER())

def signZip(appDirectory, outputFile, issuerName, manifestHashes,
            signatureHashes, pkcs7Hashes, doSign, coseAlgorithms):
    """Given a directory containing the files to package up,
    an output filename to write to, the name of the issuer of
    the signing certificate, a list of hash algorithms to use in
    the manifest file, a similar list for the signature file,
    a similar list for the pkcs#7 signature, whether or not to
    actually sign the resulting package, and a list of COSE
    signature algorithms to include, packages up the files in the
    directory and creates the output as appropriate."""
    # This ensures each manifest file starts with the magic string and
    # then a blank line.
    mfEntries = ['Manifest-Version: 1.0', '']

    with zipfile.ZipFile(outputFile, 'w') as outZip:
        for (fullPath, internalPath) in walkDirectory(appDirectory):
            with open(fullPath) as inputFile:
                contents = inputFile.read()
            outZip.writestr(internalPath, contents)

            # Add the entry to the manifest we're building
            addManifestEntry(internalPath, manifestHashes, contents, mfEntries)

        # Just exit early if we're not actually signing.
        if not doSign:
            return

        if len(coseAlgorithms) > 0:
            coseManifest = '\n'.join(mfEntries)
            outZip.writestr('META-INF/cose.manifest', coseManifest)
            addManifestEntry('META-INF/cose.manifest', manifestHashes,
                             coseManifest, mfEntries)
            signatures = map(lambda coseAlgorithm:
                coseAlgorithmToSignatureParams(coseAlgorithm, issuerName),
                coseAlgorithms)
            coseSignatureBytes = coseSig(coseManifest, [], signatures)
            outZip.writestr('META-INF/cose.sig', coseSignatureBytes)
            addManifestEntry('META-INF/cose.sig', manifestHashes,
                             coseSignatureBytes, mfEntries)

        mfContents = '\n'.join(mfEntries)
        sfContents = 'Signature-Version: 1.0\n'
        for (hashFunc, name) in signatureHashes:
            base64hash = b64encode(hashFunc(mfContents).digest())
            sfContents += '%s-Digest-Manifest: %s\n' % (name, base64hash)

        cmsSpecification = ''
        for name in pkcs7Hashes:
            hashFunc, _ = hashNameToFunctionAndIdentifier(name)
            cmsSpecification += '%s:%s\n' % (name,
                                             hashFunc(sfContents).hexdigest())
        cmsSpecification += 'signer:\n' + \
            'issuer:%s\n' % issuerName + \
            'subject:xpcshell signed app test signer\n' + \
            'extension:keyUsage:digitalSignature'
        cmsSpecificationStream = StringIO.StringIO()
        print >>cmsSpecificationStream, cmsSpecification
        cmsSpecificationStream.seek(0)
        cms = pycms.CMS(cmsSpecificationStream)
        p7 = cms.toDER()
        outZip.writestr('META-INF/A.RSA', p7)
        outZip.writestr('META-INF/A.SF', sfContents)
        outZip.writestr('META-INF/MANIFEST.MF', mfContents)

class Error(Exception):
    """Base class for exceptions in this module."""
    pass


class UnknownHashAlgorithmError(Error):
    """Helper exception type to handle unknown hash algorithms."""

    def __init__(self, name):
        super(UnknownHashAlgorithmError, self).__init__()
        self.name = name

    def __str__(self):
        return 'Unknown hash algorithm %s' % repr(self.name)


class UnknownCOSEAlgorithmError(Error):
    """Helper exception type to handle unknown COSE algorithms."""

    def __init__(self, name):
        super(UnknownCOSEAlgorithmError, self).__init__()
        self.name = name

    def __str__(self):
        return 'Unknown COSE algorithm %s' % repr(self.name)


def hashNameToFunctionAndIdentifier(name):
    if name == 'sha1':
        return (sha1, 'SHA1')
    if name == 'sha256':
        return (sha256, 'SHA256')
    raise UnknownHashAlgorithmError(name)

def main(outputFile, appPath, *args):
    """Main entrypoint. Given an already-opened file-like
    object, a path to the app directory to sign, and some
    optional arguments, signs the contents of the directory and
    writes the resulting package to the 'file'."""
    parser = argparse.ArgumentParser(description='Sign an app.')
    parser.add_argument('-n', '--no-sign', action='store_true',
                        help='Don\'t actually sign - only create zip')
    parser.add_argument('-i', '--issuer', action='store', help='Issuer name',
                        default='xpcshell signed apps test root')
    parser.add_argument('-m', '--manifest-hash', action='append',
                        help='Hash algorithms to use in manifest',
                        default=[])
    parser.add_argument('-s', '--signature-hash', action='append',
                        help='Hash algorithms to use in signature file',
                        default=[])
    parser.add_argument('-c', '--cose-sign', action='append',
                        help='Append a COSE signature with the given ' +
                             'algorithms (out of ES256, ES384, and ES512)',
                        default=[])
    group = parser.add_mutually_exclusive_group()
    group.add_argument('-p', '--pkcs7-hash', action='append',
                       help='Hash algorithms to use in PKCS#7 signature',
                       default=[])
    group.add_argument('-e', '--empty-signerInfos', action='store_true',
                       help='Emit pkcs#7 SignedData with empty signerInfos')
    parsed = parser.parse_args(args)
    if len(parsed.manifest_hash) == 0:
        parsed.manifest_hash.append('sha256')
    if len(parsed.signature_hash) == 0:
        parsed.signature_hash.append('sha256')
    if len(parsed.pkcs7_hash) == 0 and not parsed.empty_signerInfos:
        parsed.pkcs7_hash.append('sha256')
    signZip(appPath, outputFile, parsed.issuer,
            map(hashNameToFunctionAndIdentifier, parsed.manifest_hash),
            map(hashNameToFunctionAndIdentifier, parsed.signature_hash),
            parsed.pkcs7_hash, not parsed.no_sign, parsed.cose_sign)
