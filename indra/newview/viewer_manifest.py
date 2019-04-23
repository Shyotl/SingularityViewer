#!/usr/bin/env python
"""\
@file viewer_manifest.py
@author Ryan Williams
@brief Description of all installer viewer files, and methods for packaging
       them into installers for all supported platforms.

$LicenseInfo:firstyear=2006&license=viewergpl$
Second Life Viewer Source Code
Copyright (c) 2006-2009, Linden Research, Inc.

The source code in this file ("Source Code") is provided by Linden Lab
to you under the terms of the GNU General Public License, version 2.0
("GPL"), unless you have obtained a separate licensing agreement
("Other License"), formally executed by you and Linden Lab.  Terms of
the GPL can be found in doc/GPL-license.txt in this distribution, or
online at http://secondlifegrid.net/programs/open_source/licensing/gplv2

There are special exceptions to the terms and conditions of the GPL as
it is applied to this Source Code. View the full text of the exception
in the file doc/FLOSS-exception.txt in this software distribution, or
online at
http://secondlifegrid.net/programs/open_source/licensing/flossexception

By copying, modifying or distributing this software, you acknowledge
that you have read and understood your obligations described above,
and agree to abide by those obligations.

ALL LINDEN LAB SOURCE CODE IS PROVIDED "AS IS." LINDEN LAB MAKES NO
WARRANTIES, EXPRESS, IMPLIED OR OTHERWISE, REGARDING ITS ACCURACY,
COMPLETENESS OR PERFORMANCE.
$/LicenseInfo$
"""
import sys
import os.path
import errno
import re
import tarfile
import time
import random
viewer_dir = os.path.dirname(__file__)
# Add indra/lib/python to our path so we don't have to muck with PYTHONPATH.
# Put it FIRST because some of our build hosts have an ancient install of
# indra.util.llmanifest under their system Python!
sys.path.insert(0, os.path.join(viewer_dir, os.pardir, "lib", "python"))
from indra.util.llmanifest import LLManifest, main, proper_windows_path, path_ancestors, CHANNEL_VENDOR_BASE, RELEASE_CHANNEL, ManifestError
try:
    from llbase import llsd
except ImportError:
    from indra.base import llsd

class ViewerManifest(LLManifest):
    def is_packaging_viewer(self):
        # Some commands, files will only be included
        # if we are packaging the viewer on windows.
        # This manifest is also used to copy
        # files during the build (see copy_w_viewer_manifest
        # and copy_l_viewer_manifest targets)
        return 'package' in self.args['actions']

    def package_skin(self, xml, skin_dir):
        self.path(xml)
        self.path(skin_dir + "/*")
        # include the entire textures directory recursively
        if self.prefix(src=skin_dir+"/textures"):
            self.path("*/*.tga")
            self.path("*/*.j2c")
            self.path("*/*.jpg")
            self.path("*/*.png")
            self.path("*.tga")
            self.path("*.j2c")
            self.path("*.jpg")
            self.path("*.png")
            self.path("textures.xml")
            self.end_prefix(skin_dir+"/textures")

    def construct(self):
        super(ViewerManifest, self).construct()
        self.path(src="../../scripts/messages/message_template.msg", dst="app_settings/message_template.msg")
        self.path(src="../../etc/message.xml", dst="app_settings/message.xml")

        if True: #self.is_packaging_viewer():
            if self.prefix(src="app_settings"):
                self.exclude("logcontrol.xml")
                self.exclude("logcontrol-dev.xml")
                self.path("*.crt")
                self.path("*.ini")
                self.path("*.xml")
                self.path("*.db2")

                # include the entire shaders directory recursively
                self.path("shaders")

                # ... and the entire windlight directory
                self.path("windlight")

                # ... and the included spell checking dictionaries
                pkgdir = os.path.join(self.args['build'], os.pardir, 'packages')
                self.path("dictionaries")

                # include the extracted packages information (see BuildPackagesInfo.cmake)
                self.path(src=os.path.join(self.args['build'],"packages-info.txt"), dst="packages-info.txt")


                self.end_prefix("app_settings")

            if self.prefix(src="character"):
                self.path("*.llm")
                self.path("*.xml")
                self.path("*.tga")
                self.end_prefix("character")

            # Include our fonts
            if self.prefix(src=os.path.join(pkgdir, "fonts"), dst="fonts"):
                self.path("DejaVuSansCondensed*.ttf")
                self.path("DejaVuSansMono.ttf")
                self.path("DroidSans*.ttf")
                self.path("NotoSansDisplay*.ttf")
                self.path("SourceHanSans*.otf")
                self.end_prefix()

            # Include our font licenses
            if self.prefix(src="fonts"):
                self.path("*.txt")
                self.end_prefix("fonts")

            # skins
            if self.prefix(src="skins"):
                self.path("paths.xml")
                self.path("default/xui/*/*.xml")
		self.package_skin("Default.xml", "default")
		self.package_skin("dark.xml", "dark")
		self.package_skin("Gemini.xml", "gemini")

                # Local HTML files (e.g. loading screen)
                if self.prefix(src="*/html"):
                    self.path("*.png")
                    self.path("*/*/*.html")
                    self.path("*/*/*.gif")
                    self.end_prefix("*/html")

                self.end_prefix("skins")

            # Files in the newview/ directory
            self.path("gpu_table.txt")
            # The summary.json file gets left in the build directory by newview/CMakeLists.txt.
            if not self.path2basename(os.pardir, "summary.json"):
                print "No summary.json file"

    def standalone(self):
        return self.args['standalone'] == "ON"

    def grid(self):
        return self.args['grid']

    def viewer_branding_id(self):
        return self.args['branding_id']

    def channel(self):
        return self.args['channel']

    def channel_with_pkg_suffix(self):
        fullchannel=self.channel()
        if 'channel_suffix' in self.args and self.args['channel_suffix']:
            fullchannel+=' '+self.args['channel_suffix']
        return fullchannel

    def channel_variant(self):
        global CHANNEL_VENDOR_BASE
        return self.channel().replace(CHANNEL_VENDOR_BASE, "").strip()

    def channel_type(self): # returns 'release', 'beta', 'project', or 'test'
        global CHANNEL_VENDOR_BASE
        channel_qualifier=self.channel().replace(CHANNEL_VENDOR_BASE, "").lower().strip()
        if channel_qualifier.startswith('release'):
            channel_type='release'
        elif channel_qualifier.startswith('beta'):
            channel_type='beta'
        elif channel_qualifier.startswith('alpha'):
            channel_type='alpha'
        elif channel_qualifier.startswith('project'):
            channel_type='project'
        else:
            channel_type='test'
        return channel_type

    def channel_variant_app_suffix(self):
        # get any part of the compiled channel name after the CHANNEL_VENDOR_BASE
        suffix=self.channel_variant()
        # by ancient convention, we don't use Release in the app name
        if self.channel_type() == 'release':
            suffix=suffix.replace('Release', '').strip()
        # for the base release viewer, suffix will now be null - for any other, append what remains
        if len(suffix) > 0:
            suffix = "_"+ ("_".join(suffix.split()))
        # the additional_packages mechanism adds more to the installer name (but not to the app name itself)
        if 'channel_suffix' in self.args and self.args['channel_suffix']:
            suffix+='_'+("_".join(self.args['channel_suffix'].split()))
        return suffix

    def installer_base_name(self):
        global CHANNEL_VENDOR_BASE
        # a standard map of strings for replacing in the templates
        substitution_strings = {
            'channel_vendor_base' : '_'.join(CHANNEL_VENDOR_BASE.split()),
            'channel_variant_underscores':self.channel_variant_app_suffix(),
            'version_underscores' : '_'.join(self.args['version']),
            'arch':self.args['arch']
            }
        return "%(channel_vendor_base)s%(channel_variant_underscores)s_%(version_underscores)s_%(arch)s" % substitution_strings

    def app_name(self):
        global CHANNEL_VENDOR_BASE
        channel_type=self.channel_type()
        if channel_type == 'release':
            app_suffix=''
        else:
            app_suffix=self.channel_variant()
        return CHANNEL_VENDOR_BASE + ' ' + app_suffix

    def app_name_oneword(self):
        return ''.join(self.app_name().split())

    def icon_path(self):
        return "icons/" + ("default", "alpha")[self.channel_type() == "alpha"]

    def extract_names(self,src):
        try:
            contrib_file = open(src,'r')
        except IOError:
            print "Failed to open '%s'" % src
            raise
        lines = contrib_file.readlines()
        contrib_file.close()

        # All lines up to and including the first blank line are the file header; skip them
        lines.reverse() # so that pop will pull from first to last line
        while not re.match("\s*$", lines.pop()) :
            pass # do nothing

        # A line that starts with a non-whitespace character is a name; all others describe contributions, so collect the names
        names = []
        for line in lines :
            if re.match("\S", line) :
                names.append(line.rstrip())
        # It's not fair to always put the same people at the head of the list
        random.shuffle(names)
        return ', '.join(names)

class WindowsManifest(ViewerManifest):
    def is_win64(self):
        return self.args.get('arch') == "x86_64"

    def final_exe(self):
        return self.app_name_oneword()+"Viewer.exe"

    def test_msvcrt_and_copy_action(self, src, dst):
        # This is used to test a dll manifest.
        # It is used as a temporary override during the construct method
        from test_win32_manifest import test_assembly_binding
        if src and (os.path.exists(src) or os.path.islink(src)):
            # ensure that destination path exists
            self.cmakedirs(os.path.dirname(dst))
            self.created_paths.append(dst)
            if not os.path.isdir(src):
                if(self.args['configuration'].lower() == 'debug'):
                    test_assembly_binding(src, "Microsoft.VC80.DebugCRT", "8.0.50727.4053")
                else:
                    test_assembly_binding(src, "Microsoft.VC80.CRT", "8.0.50727.4053")
                self.ccopy(src,dst)
            else:
                raise Exception("Directories are not supported by test_CRT_and_copy_action()")
        else:
            print "Doesn't exist:", src

    def test_for_no_msvcrt_manifest_and_copy_action(self, src, dst):
        # This is used to test that no manifest for the msvcrt exists.
        # It is used as a temporary override during the construct method
        from test_win32_manifest import test_assembly_binding
        from test_win32_manifest import NoManifestException, NoMatchingAssemblyException
        if src and (os.path.exists(src) or os.path.islink(src)):
            # ensure that destination path exists
            self.cmakedirs(os.path.dirname(dst))
            self.created_paths.append(dst)
            if not os.path.isdir(src):
                try:
                    if(self.args['configuration'].lower() == 'debug'):
                        test_assembly_binding(src, "Microsoft.VC80.DebugCRT", "")
                    else:
                        test_assembly_binding(src, "Microsoft.VC80.CRT", "")
                    raise Exception("Unknown condition")
                except NoManifestException, err:
                    pass
                except NoMatchingAssemblyException, err:
                    pass

                self.ccopy(src,dst)
            else:
                raise Exception("Directories are not supported by test_CRT_and_copy_action()")
        else:
            print "Doesn't exist:", src

    def construct(self):
        super(WindowsManifest, self).construct()

        pkgdir = os.path.join(self.args['build'], os.pardir, 'packages')
        relpkgdir = os.path.join(pkgdir, "lib", "release")
        debpkgdir = os.path.join(pkgdir, "lib", "debug")

        if True: #self.is_packaging_viewer():
            # Find singularity-bin.exe in the 'configuration' dir, then rename it to the result of final_exe.
            self.path(src='%s/%s-bin.exe' % (self.args['configuration'],self.viewer_branding_id()), dst=self.final_exe())

        # Plugin host application
        self.path2basename(os.path.join(os.pardir,
                                        'llplugin', 'slplugin', self.args['configuration']),
                           "SLplugin.exe")

        # Get shared libs from the shared libs staging directory
        if self.prefix(src=os.path.join(os.pardir, 'sharedlibs', self.args['configuration']),
                       dst=""):

            # Get llcommon and deps. If missing assume static linkage and continue.
            try:
                self.path('llcommon.dll')
                self.path('libapr-1.dll')
                self.path('libaprutil-1.dll')
                self.path('libapriconv-1.dll')

            except RuntimeError, err:
                print err.message
                print "Skipping llcommon.dll (assuming llcommon was linked statically)"

            # Mesh 3rd party libs needed for auto LOD and collada reading
            try:
                self.path("glod.dll")
            except RuntimeError, err:
                print err.message
                print "Skipping GLOD library (assumming linked statically)"

            # Vivox runtimes
            self.path("SLVoice.exe")
            self.path("vivoxsdk.dll")
            self.path("ortp.dll")
            self.path("libsndfile-1.dll")
            self.path("vivoxoal.dll")
            self.path("ca-bundle.crt")

            # Hunspell
            self.path("libhunspell.dll")

            # For google-perftools tcmalloc allocator.
            try:
                if self.args['configuration'].lower() == 'debug':
                    self.path('libtcmalloc_minimal-debug.dll')
                else:
                    self.path('libtcmalloc_minimal.dll')
            except:
                print "Skipping libtcmalloc_minimal.dll"

            self.end_prefix()

        self.path(src="licenses-win32.txt", dst="licenses.txt")
        self.path("featuretable.txt")

        # Plugins - FilePicker
        if self.prefix(src='../plugins/filepicker/%s' % self.args['configuration'], dst="llplugin"):
            self.path("basic_plugin_filepicker.dll")
            self.end_prefix()

        # Media plugins - QuickTime
        if self.prefix(src='../plugins/quicktime/%s' % self.args['configuration'], dst="llplugin"):
            self.path("media_plugin_quicktime.dll")
            self.end_prefix()

        # Media plugins - CEF
        if self.prefix(src='../plugins/cef/%s' % self.args['configuration'], dst="llplugin"):
            self.path("media_plugin_cef.dll")
            self.end_prefix()

        # CEF runtime files - debug
        # CEF runtime files - not debug (release, relwithdebinfo etc.)
        config = 'debug' if self.args['configuration'].lower() == 'debug' else 'release'
        if self.prefix(src=os.path.join(pkgdir, 'bin', config), dst="llplugin"):
                self.path("chrome_elf.dll")
                self.path("d3dcompiler_43.dll")
                self.path("d3dcompiler_47.dll")
                self.path("libcef.dll")
                self.path("libEGL.dll")
                self.path("libGLESv2.dll")
                self.path("dullahan_host.exe")
                self.path("natives_blob.bin")
                self.path("snapshot_blob.bin")
                self.path("v8_context_snapshot.bin")
                self.end_prefix()

        if self.prefix(src=os.path.join(pkgdir, 'bin', config, 'swiftshader'), dst=os.path.join("llplugin", 'swiftshader')):
            self.path("libEGL.dll")
            self.path("libGLESv2.dll")
            self.end_prefix()

        # CEF files common to all configurations
        if self.prefix(src=os.path.join(pkgdir, 'resources'), dst="llplugin"):
            self.path("cef.pak")
            self.path("cef_100_percent.pak")
            self.path("cef_200_percent.pak")
            self.path("cef_extensions.pak")
            self.path("devtools_resources.pak")
            self.path("icudtl.dat")
            self.end_prefix()

        if self.prefix(src=os.path.join(pkgdir, 'resources', 'locales'), dst=os.path.join('llplugin', 'locales')):
            self.path("am.pak")
            self.path("ar.pak")
            self.path("bg.pak")
            self.path("bn.pak")
            self.path("ca.pak")
            self.path("cs.pak")
            self.path("da.pak")
            self.path("de.pak")
            self.path("el.pak")
            self.path("en-GB.pak")
            self.path("en-US.pak")
            self.path("es-419.pak")
            self.path("es.pak")
            self.path("et.pak")
            self.path("fa.pak")
            self.path("fi.pak")
            self.path("fil.pak")
            self.path("fr.pak")
            self.path("gu.pak")
            self.path("he.pak")
            self.path("hi.pak")
            self.path("hr.pak")
            self.path("hu.pak")
            self.path("id.pak")
            self.path("it.pak")
            self.path("ja.pak")
            self.path("kn.pak")
            self.path("ko.pak")
            self.path("lt.pak")
            self.path("lv.pak")
            self.path("ml.pak")
            self.path("mr.pak")
            self.path("ms.pak")
            self.path("nb.pak")
            self.path("nl.pak")
            self.path("pl.pak")
            self.path("pt-BR.pak")
            self.path("pt-PT.pak")
            self.path("ro.pak")
            self.path("ru.pak")
            self.path("sk.pak")
            self.path("sl.pak")
            self.path("sr.pak")
            self.path("sv.pak")
            self.path("sw.pak")
            self.path("ta.pak")
            self.path("te.pak")
            self.path("th.pak")
            self.path("tr.pak")
            self.path("uk.pak")
            self.path("vi.pak")
            self.path("zh-CN.pak")
            self.path("zh-TW.pak")
            self.end_prefix()


        if not self.is_packaging_viewer():
            self.package_file = "copied_deps"

    def nsi_file_commands(self, install=True):
        def wpath(path):
            if path.endswith('/') or path.endswith(os.path.sep):
                path = path[:-1]
            path = path.replace('/', '\\')
            return path

        result = ""
        dest_files = [pair[1] for pair in self.file_list if pair[0] and os.path.isfile(pair[1])]
        # sort deepest hierarchy first
        dest_files.sort(lambda a,b: cmp(a.count(os.path.sep),b.count(os.path.sep)) or cmp(a,b))
        dest_files.reverse()
        out_path = None
        for pkg_file in dest_files:
            rel_file = os.path.normpath(pkg_file.replace(self.get_dst_prefix()+os.path.sep,''))
            installed_dir = wpath(os.path.join('$INSTDIR', os.path.dirname(rel_file)))
            pkg_file = wpath(os.path.normpath(pkg_file))
            if installed_dir != out_path:
                if install:
                    out_path = installed_dir
                    result += 'SetOutPath ' + out_path + '\n'
            if install:
                result += 'File ' + pkg_file + '\n'
            else:
                result += 'Delete ' + wpath(os.path.join('$INSTDIR', rel_file)) + '\n'

        # at the end of a delete, just rmdir all the directories
        if not install:
            deleted_file_dirs = [os.path.dirname(pair[1].replace(self.get_dst_prefix()+os.path.sep,'')) for pair in self.file_list]
            # find all ancestors so that we don't skip any dirs that happened to have no non-dir children
            deleted_dirs = []
            for d in deleted_file_dirs:
                deleted_dirs.extend(path_ancestors(d))
            # sort deepest hierarchy first
            deleted_dirs.sort(lambda a,b: cmp(a.count(os.path.sep),b.count(os.path.sep)) or cmp(a,b))
            deleted_dirs.reverse()
            prev = None
            for d in deleted_dirs:
                if d != prev:   # skip duplicates
                    result += 'RMDir ' + wpath(os.path.join('$INSTDIR', os.path.normpath(d))) + '\n'
                prev = d

        return result
	
    def sign_command(self, *argv):
        return [
            "signtool.exe", "sign", "/v",
            "/n", self.args['signature'],
            "/p", os.environ['VIEWER_SIGNING_PWD'],
            "/d","%s" % self.channel(),
            "/t","http://timestamp.comodoca.com/authenticode"
        ] + list(argv)
	
    def sign(self, *argv):
        subprocess.check_call(self.sign_command(*argv))

    def package_finish(self):
        if 'signature' in self.args and 'VIEWER_SIGNING_PWD' in os.environ:
            try:
                self.sign(self.args['configuration']+"\\"+self.final_exe())
                self.sign(self.args['configuration']+"\\SLPlugin.exe")
                self.sign(self.args['configuration']+"\\SLVoice.exe")
            except:
                print "Couldn't sign binaries. Tried to sign %s" % self.args['configuration'] + "\\" + self.final_exe()
		
        # a standard map of strings for replacing in the templates
        substitution_strings = {
            'version' : '.'.join(self.args['version']),
            'version_short' : '.'.join(self.args['version'][:-1]),
            'version_dashes' : '-'.join(self.args['version']),
            'final_exe' : self.final_exe(),
            'flags':'',
            'app_name':self.app_name(),
            'app_name_oneword':self.app_name_oneword()
            }

        installer_file = self.installer_base_name() + '_Setup.exe'
        substitution_strings['installer_file'] = installer_file

        version_vars = """
        !define INSTEXE  "%(final_exe)s"
        !define VERSION "%(version_short)s"
        !define VERSION_LONG "%(version)s"
        !define VERSION_DASHES "%(version_dashes)s"
        """ % substitution_strings

        if self.channel_type() == 'release':
            substitution_strings['caption'] = CHANNEL_VENDOR_BASE
        else:
            substitution_strings['caption'] = self.app_name() + ' ${VERSION}'

        inst_vars_template = """
            !define INSTOUTFILE "%(installer_file)s"
            !define INSTEXE  "%(final_exe)s"
            !define APPNAME   "%(app_name)s"
            !define APPNAMEONEWORD   "%(app_name_oneword)s"
            !define VERSION "%(version_short)s"
            !define VERSION_LONG "%(version)s"
            !define VERSION_DASHES "%(version_dashes)s"
            !define URLNAME   "secondlife"
            !define CAPTIONSTR "%(caption)s"
            !define VENDORSTR "Singularity Viewer Project"
            """

        tempfile = "%s_setup_tmp.nsi" % self.viewer_branding_id()
        # the following replaces strings in the nsi template
        # it also does python-style % substitution
        self.replace_in("installers/windows/installer_template.nsi", tempfile, {
                "%%VERSION%%":version_vars,
                "%%SOURCE%%":self.get_src_prefix(),
                "%%INST_VARS%%":inst_vars_template % substitution_strings,
                "%%INSTALL_FILES%%":self.nsi_file_commands(True),
                "%%DELETE_FILES%%":self.nsi_file_commands(False),
                "%%WIN64_BIN_BUILD%%":"!define WIN64_BIN_BUILD 1" if self.is_win64() else "",
                })

        # We use the Unicode version of NSIS, available from
        # http://www.scratchpaper.com/
        try:
            import _winreg as reg
            NSIS_path = reg.QueryValue(reg.HKEY_LOCAL_MACHINE, r"SOFTWARE\NSIS") + '\\makensis.exe'
            self.run_command([proper_windows_path(NSIS_path), self.dst_path_of(tempfile)])
        except:
            try:
                NSIS_path = os.environ['ProgramFiles'] + '\\NSIS\\makensis.exe'
                self.run_command([proper_windows_path(NSIS_path), self.dst_path_of(tempfile)])
            except:
                NSIS_path = os.environ['ProgramFiles(X86)'] + '\\NSIS\\makensis.exe'
                self.run_command([proper_windows_path(NSIS_path),self.dst_path_of(tempfile)])


        # self.remove(self.dst_path_of(tempfile))
        if 'signature' in self.args and 'VIEWER_SIGNING_PWD' in os.environ:
            try:
                self.sign(self.args['configuration'] + "\\" + substitution_strings['installer_file'])
            except: 
                print "Couldn't sign windows installer. Tried to sign %s" % self.args['configuration'] + "\\" + substitution_strings['installer_file']

        self.created_path(self.dst_path_of(installer_file))
        self.package_file = installer_file


class Windows_i686_Manifest(WindowsManifest):
    def construct(self):
        super(Windows_i686_Manifest, self).construct()

        # Get shared libs from the shared libs staging directory
        if self.prefix(src=os.path.join(os.pardir, 'sharedlibs', self.args['configuration']),
                       dst=""):

            # Security
            self.path("libcrypto-1_1.dll")
            self.path("libssl-1_1.dll")

            # Get fmod studio dll, continue if missing
            try:
                if self.args['configuration'].lower() == 'debug':
                    self.path("fmodL.dll")
                else:
                    self.path("fmod.dll")
            except:
                print "Skipping fmodstudio audio library(assuming other audio engine)"

            self.end_prefix()

        if self.prefix(src=os.path.join(self.args['build'], os.pardir, 'packages', 'bin'), dst="redist"):
            self.path("vc_redist.x86.exe")
            self.end_prefix()


class Windows_x86_64_Manifest(WindowsManifest):
    def construct(self):
        super(Windows_x86_64_Manifest, self).construct()

        # Get shared libs from the shared libs staging directory
        if self.prefix(src=os.path.join(os.pardir, 'sharedlibs', self.args['configuration']),
                       dst=""):

            # Security
            self.path("libcrypto-1_1-x64.dll")
            self.path("libssl-1_1-x64.dll")

            # Get fmodstudio dll, continue if missing
            try:
                if self.args['configuration'].lower() == 'debug':
                    self.path("fmodL64.dll")
                else:
                    self.path("fmod64.dll")
            except:
                print "Skipping fmodstudio audio library(assuming other audio engine)"

            self.end_prefix()

        if self.prefix(src=os.path.join(self.args['build'], os.pardir, 'packages', 'bin'), dst="redist"):
            self.path("vc_redist.x64.exe")
            self.end_prefix()


class DarwinManifest(ViewerManifest):
    def construct(self):
        # copy over the build result (this is a no-op if run within the xcode script)
        self.path(self.args['configuration'] + "/" + self.app_name() + ".app", dst="")

        if self.prefix(src="", dst="Contents"):  # everything goes in Contents

            # most everything goes in the Resources directory
            if self.prefix(src="", dst="Resources"):
                super(DarwinManifest, self).construct()

                if self.prefix("cursors_mac"):
                    self.path("*.tif")
                    self.end_prefix("cursors_mac")

                self.path("licenses-mac.txt", dst="licenses.txt")
                self.path("featuretable_mac.txt")

                icon_path = self.icon_path()
                if self.prefix(src=icon_path, dst="") :
                    self.path("%s.icns" % self.viewer_branding_id())
                    self.end_prefix(icon_path)

                self.path("SecondLife.nib")

                # Translations
                self.path("English.lproj")
                self.path("German.lproj")
                self.path("Japanese.lproj")
                self.path("Korean.lproj")
                self.path("da.lproj")
                self.path("es.lproj")
                self.path("fr.lproj")
                self.path("hu.lproj")
                self.path("it.lproj")
                self.path("nl.lproj")
                self.path("pl.lproj")
                self.path("pt.lproj")
                self.path("ru.lproj")
                self.path("tr.lproj")
                self.path("uk.lproj")
                self.path("zh-Hans.lproj")

                libdir = "../packages/lib/release"
                alt_libdir = "../packages/libraries/universal-darwin/lib/release"
                # dylibs is a list of all the .dylib files we expect to need
                # in our bundled sub-apps. For each of these we'll create a
                # symlink from sub-app/Contents/Resources to the real .dylib.
                # Need to get the llcommon dll from any of the build directories as well.
                libfile = "libllcommon.dylib"

                dylibs = self.path_optional(self.find_existing_file(os.path.join(os.pardir,
                                                               "llcommon",
                                                               self.args['configuration'],
                                                               libfile),
                                                               os.path.join(libdir, libfile)),
                                       dst=libfile)

                if self.prefix(src=libdir, alt_build=alt_libdir, dst=""):
                    for libfile in (
                                    "libapr-1.0.dylib",
                                    "libaprutil-1.0.dylib",
                                    "libcollada14dom.dylib",
                                    "libexpat.1.5.2.dylib",
                                    "libexception_handler.dylib",
                                    "libGLOD.dylib",
                                    "libhunspell-1.3.0.dylib",
                                    "libndofdev.dylib",
                                    ):
                        dylibs += self.path_optional(libfile)

                    for libfile in (
                                'libortp.dylib',
                                'libsndfile.dylib',
                                'libvivoxoal.dylib',
                                'libvivoxsdk.dylib',
                                'libvivoxplatform.dylib',
                                'ca-bundle.crt',
                                'SLVoice'
                                ):
                        self.path(libfile)
                    
                    self.end_prefix()

                if self.prefix(src= '', alt_build=libdir):
                    dylibs += self.add_extra_libraries()
                    self.end_prefix()

                # our apps
                for app_bld_dir, app in (#("mac_crash_logger", "mac-crash-logger.app"),
                                         # plugin launcher
                                         (os.path.join("llplugin", "slplugin"), "SLPlugin.app"),
                                         ):
                    self.path2basename(os.path.join(os.pardir,
                                                    app_bld_dir, self.args['configuration']),
                                       app)

                    # our apps dependencies on shared libs
                    # for each app, for each dylib we collected in dylibs,
                    # create a symlink to the real copy of the dylib.
                    resource_path = self.dst_path_of(os.path.join(app, "Contents", "Resources"))
                    for libfile in dylibs:
                        src = os.path.join(os.pardir, os.pardir, os.pardir, libfile)
                        dst = os.path.join(resource_path, libfile)
                        try:
                            symlinkf(src, dst)
                        except OSError as err:
                            print "Can't symlink %s -> %s: %s" % (src, dst, err)

                # Dullahan helper apps go inside AlchemyPlugin.app
                if self.prefix(src="", dst="AlchemyPlugin.app/Contents/Frameworks"):
                    helperappfile = 'DullahanHelper.app'
                    self.path2basename(relbinpkgdir, helperappfile)

                    pluginframeworkpath = self.dst_path_of('Chromium Embedded Framework.framework');
                    # Putting a Frameworks directory under Contents/MacOS
                    # isn't canonical, but the path baked into Dullahan
                    # Helper.app/Contents/MacOS/DullahanHelper is:
                    # @executable_path/Frameworks/Chromium Embedded Framework.framework/Chromium Embedded Framework
                    # (notice, not @executable_path/../Frameworks/etc.)
                    # So we'll create a symlink (below) from there back to the
                    # Frameworks directory nested under SLPlugin.app.
                    helperframeworkpath = \
                        self.dst_path_of('DullahanHelper.app/Contents/MacOS/'
                                         'Frameworks/Chromium Embedded Framework.framework')

                    helperexecutablepath = self.dst_path_of('DullahanHelper.app/Contents/MacOS/DullahanHelper')
                    self.run_command(['install_name_tool', '-change',
                                     '@rpath/Frameworks/Chromium Embedded Framework.framework/Chromium Embedded Framework',
                                     '@executable_path/Frameworks/Chromium Embedded Framework.framework/Chromium Embedded Framework', helperexecutablepath])

                    self.end_prefix()

                # plugins
                if self.prefix(src="", dst="llplugin"):
                    self.path2basename("../media_plugins/quicktime/" + self.args['configuration'],
                                       "media_plugin_quicktime.dylib")
                    self.path2basename("../media_plugins/cef/" + self.args['configuration'],
                                       "media_plugin_cef.dylib")

                    self.end_prefix("llplugin")

                # command line arguments for connecting to the proper grid
                self.put_in_file(self.flags_list(), 'arguments.txt')

                self.end_prefix("Resources")

                # CEF framework goes inside Second Life.app/Contents/Frameworks
                if self.prefix(src="", dst="Frameworks"):
                    frameworkfile="Chromium Embedded Framework.framework"
                    self.path2basename(relpkgdir, frameworkfile)
                    self.end_prefix("Frameworks")

                # This code constructs a relative path from the
                # target framework folder back to the location of the symlink.
                # It needs to be relative so that the symlink still works when
                # (as is normal) the user moves the app bundle out of the DMG
                # and into the /Applications folder. Note we also call 'raise'
                # to terminate the process if we get an error since without
                # this symlink, Second Life web media can't possibly work.
                # Real Framework folder:
                #   Alchemy.app/Contents/Frameworks/Chromium Embedded Framework.framework/
                # Location of symlink and why it's relative 
                #   Alchemy.app/Contents/Resources/AlchemyPlugin.app/Contents/Frameworks/Chromium Embedded Framework.framework/
                # Real Frameworks folder, with the symlink inside the bundled SLPlugin.app (and why it's relative)
                #   <top level>.app/Contents/Frameworks/Chromium Embedded Framework.framework/
                #   <top level>.app/Contents/Resources/AlchemyPlugin.app/Contents/Frameworks/Chromium Embedded Framework.framework ->
                # It might seem simpler just to create a symlink Frameworks to
                # the parent of Chromimum Embedded Framework.framework. But
                # that would create a symlink cycle, which breaks our
                # packaging step. So make a symlink from Chromium Embedded
                # Framework.framework to the directory of the same name, which
                # is NOT an ancestor of the symlink.
                frameworkpath = os.path.join(os.pardir, os.pardir, os.pardir, 
                                             os.pardir, "Frameworks",
                                             "Chromium Embedded Framework.framework")
                try:
                    # from AlchemyPlugin.app/Contents/Frameworks/Chromium Embedded
                    # Framework.framework back to
                    # Alchemy.app/Contents/Frameworks/Chromium Embedded Framework.framework
                    origin, target = pluginframeworkpath, frameworkpath
                    self.symlinkf(target, origin)
                    # from AlchemyPlugin.app/Contents/Frameworks/Dullahan
                    # Helper.app/Contents/MacOS/Frameworks/Chromium Embedded
                    # Framework.framework back to
                    # AlchemyPlugin.app/Contents/Frameworks/Chromium Embedded Framework.framework
                    self.cmakedirs(os.path.dirname(helperframeworkpath))
                    origin = helperframeworkpath
                    target = os.path.join(os.pardir, frameworkpath)
                    self.symlinkf(target, origin)
                except OSError as err:
                    print "Can't symlink %s -> %s: %s" % (origin, target, err)
                    raise

            self.end_prefix("Contents")

        # NOTE: the -S argument to strip causes it to keep enough info for
        # annotated backtraces (i.e. function names in the crash log).  'strip' with no
        # arguments yields a slightly smaller binary but makes crash logs mostly useless.
        # This may be desirable for the final release.  Or not.
        if self.buildtype().lower()=='release':
            if ("package" in self.args['actions'] or
                "unpacked" in self.args['actions']):
                self.run_command('strip -S "%(viewer_binary)s"' %
                                 { 'viewer_binary' : self.dst_path_of('Contents/MacOS/'+self.app_name())})

    def app_name(self):
        return self.channel_oneword()

    def package_finish(self):
        channel_standin = self.app_name()
        if not self.default_channel_for_brand():
            channel_standin = self.channel()

        # Sign the app if we have a key.
        try:
            signing_password = os.environ['VIEWER_SIGNING_PASSWORD']
        except KeyError:
            print "Skipping code signing"
            pass
        else:
            home_path = os.environ['HOME']

            self.run_command('security unlock-keychain -p "%s" "%s/Library/Keychains/viewer.keychain"' % (signing_password, home_path))
            signed=False
            sign_attempts=3
            sign_retry_wait=15
            while (not signed) and (sign_attempts > 0):
                try:
                    sign_attempts-=1;
                    self.run_command('codesign --verbose --force --timestamp --keychain "%(home_path)s/Library/Keychains/viewer.keychain" -s %(identity)r -f %(bundle)r' % {
                            'home_path' : home_path,
                            'identity': os.environ['VIEWER_SIGNING_KEY'],
                            'bundle': self.get_dst_prefix()
                    })
                    signed=True
                except:
                    if sign_attempts:
                        print >> sys.stderr, "codesign failed, waiting %d seconds before retrying" % sign_retry_wait
                        time.sleep(sign_retry_wait)
                        sign_retry_wait*=2
                    else:
                        print >> sys.stderr, "Maximum codesign attempts exceeded; giving up"
                        raise

        imagename=self.installer_prefix() + '_'.join(self.args['version'])

        # See Ambroff's Hack comment further down if you want to create new bundles and dmg
        volname=self.app_name() + " Installer"  # DO NOT CHANGE without checking Ambroff's Hack comment further down

        if self.default_channel_for_brand():
            if not self.default_grid():
                # beta case
                imagename = imagename + '_' + self.args['grid'].upper()
        else:
            # first look, etc
            imagename = imagename + '_' + self.channel_oneword().upper()

        sparsename = imagename + ".sparseimage"
        finalname = imagename + ".dmg"
        # make sure we don't have stale files laying about
        self.remove(sparsename, finalname)

        self.run_command('hdiutil create "%(sparse)s" -volname "%(vol)s" -fs HFS+ -type SPARSE -megabytes 700 -layout SPUD' % {
                'sparse':sparsename,
                'vol':volname})

        # mount the image and get the name of the mount point and device node
        hdi_output = self.run_command('hdiutil attach -private "' + sparsename + '"')
        devfile = re.search("/dev/disk([0-9]+)[^s]", hdi_output).group(0).strip()
        volpath = re.search('HFS\s+(.+)', hdi_output).group(1).strip()

        # Copy everything in to the mounted .dmg

        if self.default_channel_for_brand() and not self.default_grid():
            app_name = self.app_name() + " " + self.args['grid']
        else:
            app_name = channel_standin.strip()

        # Hack:
        # Because there is no easy way to coerce the Finder into positioning
        # the app bundle in the same place with different app names, we are
        # adding multiple .DS_Store files to svn. There is one for release,
        # one for release candidate and one for first look. Any other channels
        # will use the release .DS_Store, and will look broken.
        # - Ambroff 2008-08-20
                # Added a .DS_Store for snowglobe - Merov 2009-06-17

                # We have a single branded installer for all snowglobe channels so snowglobe logic is a bit different
        if (self.app_name()=="Snowglobe"):
            dmg_template = os.path.join ('installers', 'darwin', 'snowglobe-dmg')
        else:
            dmg_template = os.path.join(
                'installers',
                'darwin',
                '%s-dmg' % "".join(self.channel_unique().split()).lower())

        if not os.path.exists (self.src_path_of(dmg_template)):
            dmg_template = os.path.join ('installers', 'darwin', 'release-dmg')

        
        for s,d in {self.build_path_of(self.get_dst_prefix()):app_name + ".app",
                    self.src_path_of(os.path.join(dmg_template, "_VolumeIcon.icns")): ".VolumeIcon.icns",
                    self.src_path_of(os.path.join(dmg_template, "background.jpg")): "background.jpg",
                    self.src_path_of(os.path.join(dmg_template, "_DS_Store")): ".DS_Store"}.items():
            print "Copying to dmg", s, d
            self.copy_action(s, os.path.join(volpath, d))

        # Hide the background image, DS_Store file, and volume icon file (set their "visible" bit)
        self.run_command('SetFile -a V "' + os.path.join(volpath, ".VolumeIcon.icns") + '"')
        self.run_command('SetFile -a V "' + os.path.join(volpath, "background.jpg") + '"')
        self.run_command('SetFile -a V "' + os.path.join(volpath, ".DS_Store") + '"')

        # Create the alias file (which is a resource file) from the .r
        self.run_command('rez "' + self.src_path_of("installers/darwin/release-dmg/Applications-alias.r") + '" -o "' + os.path.join(volpath, "Applications") + '"')

        # Set the alias file's alias and custom icon bits
        self.run_command('SetFile -a AC "' + os.path.join(volpath, "Applications") + '"')

        # Set the disk image root's custom icon bit
        self.run_command('SetFile -a C "' + volpath + '"')

        # Unmount the image
        self.run_command('hdiutil detach -force "' + devfile + '"')

        print "Converting temp disk image to final disk image"
        self.run_command('hdiutil convert "%(sparse)s" -format UDZO -imagekey zlib-level=9 -o "%(final)s"' % {'sparse':sparsename, 'final':finalname})
        # get rid of the temp file
        self.package_file = finalname
        self.remove(sparsename)

class LinuxManifest(ViewerManifest):
    def is_packaging_viewer(self):
        super(LinuxManifest, self).is_packaging_viewer()
        return True # We always want a packaged viewer even without archive.

    def do(self, *actions):
        super(LinuxManifest, self).do(*actions)
        if not 'package' in self.actions:
            self.package_finish() # Always finish the package.
        else:
            # package_finish() was called by super.do() so just create the TAR.
            self.create_archive()
        return self.file_list
    
    def construct(self):
        import shutil
        shutil.rmtree("./packaged/app_settings/shaders", ignore_errors=True);
        super(LinuxManifest, self).construct()

        self.path("licenses-linux.txt","licenses.txt")

        pkgdir = os.path.join(self.args['build'], os.pardir, 'packages')
        relpkgdir = os.path.join(pkgdir, "lib", "release")
        debpkgdir = os.path.join(pkgdir, "lib", "debug")

        if self.prefix("linux_tools", dst=""):
            self.path("client-readme.txt","README-linux.txt")
            self.path("client-readme-voice.txt","README-linux-voice.txt")
            self.path("client-readme-joystick.txt","README-linux-joystick.txt")
            self.path("wrapper.sh",self.viewer_branding_id())
            if self.prefix(src="", dst="etc"):
                self.path("handle_secondlifeprotocol.sh")
                self.path("register_secondlifeprotocol.sh")
                self.path("refresh_desktop_app_entry.sh")
                self.path("launch_url.sh")
                self.end_prefix("etc")
            self.path("install.sh")
            self.end_prefix("linux_tools")

        # Create an appropriate gridargs.dat for this package, denoting required grid.
        # self.put_in_file(self.flags_list(), 'gridargs.dat')

        if self.prefix(src="", dst="bin"):
            self.path("%s-bin"%self.viewer_branding_id(),"do-not-directly-run-%s-bin"%self.viewer_branding_id())
            self.path2basename("../llplugin/slplugin", "SLPlugin")
            self.end_prefix("bin")

        if self.prefix("res-sdl"):
            self.path("*")
            # recurse
            self.end_prefix("res-sdl")

        # Get the icons based on the channel type
        icon_path = self.icon_path()
        print "DEBUG: icon_path '%s'" % icon_path
        if self.prefix(src=icon_path, dst="") :
            self.path("viewer.png","viewer_icon.png")
            if self.prefix(src="",dst="res-sdl") :
                self.path("viewer_256.BMP","viewer_icon.BMP")
                self.end_prefix("res-sdl")
            self.end_prefix(icon_path)

        # plugins
        if self.prefix(src="", dst="bin/llplugin"):
            self.path2basename("../plugins/filepicker", "libbasic_plugin_filepicker.so")
            self.path("../plugins/gstreamer010/libmedia_plugin_gstreamer010.so", "libmedia_plugin_gstreamer.so")
            self.path("../plugins/cef/libmedia_plugin_cef.so", "libmedia_plugin_cef.so")
            self.end_prefix("bin/llplugin")

        # CEF files 
        if self.prefix(src=os.path.join(pkgdir, 'bin', 'release'), dst="bin"):
            self.path("chrome-sandbox")
            self.path("dullahan_host")
            self.path("natives_blob.bin")
            self.path("snapshot_blob.bin")
            self.path("v8_context_snapshot.bin")
            self.end_prefix()

        if self.prefix(src=os.path.join(pkgdir, 'resources'), dst="bin"):
            self.path("cef.pak")
            self.path("cef_extensions.pak")
            self.path("cef_100_percent.pak")
            self.path("cef_200_percent.pak")
            self.path("devtools_resources.pak")
            self.path("icudtl.dat")
            self.end_prefix()

        if self.prefix(src=os.path.join(pkgdir, 'resources', 'locales'), dst=os.path.join('bin', 'locales')):
            self.path("am.pak")
            self.path("ar.pak")
            self.path("bg.pak")
            self.path("bn.pak")
            self.path("ca.pak")
            self.path("cs.pak")
            self.path("da.pak")
            self.path("de.pak")
            self.path("el.pak")
            self.path("en-GB.pak")
            self.path("en-US.pak")
            self.path("es.pak")
            self.path("es-419.pak")
            self.path("et.pak")
            self.path("fa.pak")
            self.path("fi.pak")
            self.path("fil.pak")
            self.path("fr.pak")
            self.path("gu.pak")
            self.path("he.pak")
            self.path("hi.pak")
            self.path("hr.pak")
            self.path("hu.pak")
            self.path("id.pak")
            self.path("it.pak")
            self.path("ja.pak")
            self.path("kn.pak")
            self.path("ko.pak")
            self.path("lt.pak")
            self.path("lv.pak")
            self.path("ml.pak")
            self.path("mr.pak")
            self.path("ms.pak")
            self.path("nb.pak")
            self.path("nl.pak")
            self.path("pl.pak")
            self.path("pt-BR.pak")
            self.path("pt-PT.pak")
            self.path("ro.pak")
            self.path("ru.pak")
            self.path("sk.pak")
            self.path("sl.pak")
            self.path("sr.pak")
            self.path("sv.pak")
            self.path("sw.pak")
            self.path("ta.pak")
            self.path("te.pak")
            self.path("th.pak")
            self.path("tr.pak")
            self.path("uk.pak")
            self.path("vi.pak")
            self.path("zh-CN.pak")
            self.path("zh-TW.pak")
            self.end_prefix()

        # llcommon
        if not self.path("../llcommon/libllcommon.so", "lib/libllcommon.so"):
            print "Skipping llcommon.so (assuming llcommon was linked statically)"

        self.path("featuretable_linux.txt")

    def package_finish(self):
        self.strip_binaries()
        # Fix access permissions
        self.run_command("""
                find %(dst)s -type d | xargs --no-run-if-empty chmod 755;
                find %(dst)s -type f -perm 0700 | xargs --no-run-if-empty chmod 0755;
                find %(dst)s -type f -perm 0500 | xargs --no-run-if-empty chmod 0555;
                find %(dst)s -type f -perm 0600 | xargs --no-run-if-empty chmod 0644;
                find %(dst)s -type f -perm 0400 | xargs --no-run-if-empty chmod 0444;
                true""" %  {'dst':self.get_dst_prefix() })

    def strip_binaries(self):
        print "* Going strip-crazy on the packaged binaries"
        # makes some small assumptions about our packaged dir structure
        self.run_command(r"find %(d)r/lib %(d)r/lib32 %(d)r/lib64 -type f \! -name update_install | xargs --no-run-if-empty strip -S" % {'d': self.get_dst_prefix()} )
        self.run_command(r"find %(d)r/bin -executable -type f \! -name update_install | xargs --no-run-if-empty strip -S" % {'d': self.get_dst_prefix()} )

    def create_archive(self):
        installer_name = self.installer_base_name()
        # temporarily move directory tree so that it has the right
        # name in the tarfile
        self.run_command("mv %(dst)s %(inst)s" % {
            'dst': self.get_dst_prefix(),
            'inst': self.build_path_of(installer_name)})
        try:
            # --numeric-owner hides the username of the builder for
            # security etc.
            self.run_command('tar -C %(dir)s --numeric-owner -cJf '
                             '%(inst_path)s.tar.xz %(inst_name)s' % {
                    'dir': self.get_build_prefix(),
                    'inst_name': installer_name,
                    'inst_path':self.build_path_of(installer_name)})
        finally:
            self.run_command("mv %(inst)s %(dst)s" % {
                'dst': self.get_dst_prefix(),
                'inst': self.build_path_of(installer_name)})
            self.package_file = installer_name + '.tar.xz'


class Linux_i686_Manifest(LinuxManifest):
    def construct(self):
        super(Linux_i686_Manifest, self).construct()

        pkgdir = os.path.join(self.args['build'], os.pardir, 'packages')
        relpkgdir = os.path.join(pkgdir, "lib", "release")
        debpkgdir = os.path.join(pkgdir, "lib", "debug")

        if (not self.standalone()) and self.prefix(src=relpkgdir, dst="lib"):
            self.path("libapr-1.so*")
            self.path("libaprutil-1.so*")
            self.path("libcef.so")
            self.path("libexpat.so.*")
            self.path("libGLOD.so")
            self.path("libSDL-1.2.so.*")
            self.path("libalut.so")
            self.path("libopenal.so.1")

            self.path("libtcmalloc_minimal.so.0")
            self.path("libtcmalloc_minimal.so.0.2.2")
            self.end_prefix("lib")

        # Vivox runtimes
        if self.prefix(src=relpkgdir, dst="bin"):
            self.path("SLVoice")
            self.end_prefix("bin")
        if self.prefix(src=relpkgdir, dst="lib"):
            self.path("libortp.so")
            self.path("libsndfile.so.1")
            self.path("libvivoxoal.so.1")
            self.path("libvivoxsdk.so")
            self.path("libvivoxplatform.so")
            self.end_prefix("lib")


class Linux_x86_64_Manifest(LinuxManifest):
    def construct(self):
        super(Linux_x86_64_Manifest, self).construct()

        pkgdir = os.path.join(self.args['build'], os.pardir, 'packages')
        relpkgdir = os.path.join(pkgdir, "lib", "release")
        debpkgdir = os.path.join(pkgdir, "lib", "debug")

        if (not self.standalone()) and self.prefix(relpkgdir, dst="lib64"):
            self.path("libapr-1.so*")
            self.path("libaprutil-1.so*")
            self.path("libexpat.so*")
            self.path("libGLOD.so")
            self.path("libSDL-1.2.so*")
            self.path("libhunspell-1.3.so*")
            self.path("libalut.so*")
            self.path("libopenal.so*")
            self.path("libfreetype.so*")

            try:
                self.path("libtcmalloc.so*") #formerly called google perf tools
                pass
            except:
                print "tcmalloc files not found, skipping"
                pass

            try:
                self.path("libfmod.so*")
                pass
            except:
                print "Skipping libfmod.so - not found"
                pass

            self.end_prefix("lib64")

        # Vivox runtimes
        if self.prefix(src=relpkgdir, dst="bin"):
            self.path("SLVoice")
            self.end_prefix("bin")
        if self.prefix(src=relpkgdir, dst="lib32"):
            self.path("libortp.so")
            self.path("libsndfile.so.1")
            self.path("libvivoxoal.so.1")
            self.path("libvivoxsdk.so")
            self.path("libvivoxplatform.so")
            self.end_prefix("lib32")

        # plugin runtime
        if self.prefix(src=relpkgdir, dst="lib64"):
            self.path("libcef.so")
            self.end_prefix("lib64")


################################################################

def symlinkf(src, dst):
    """
    Like ln -sf, but uses os.symlink() instead of running ln.
    """
    try:
        os.symlink(src, dst)
    except OSError, err:
        if err.errno != errno.EEXIST:
            raise
        # We could just blithely attempt to remove and recreate the target
        # file, but that strategy doesn't work so well if we don't have
        # permissions to remove it. Check to see if it's already the
        # symlink we want, which is the usual reason for EEXIST.
        if not (os.path.islink(dst) and os.readlink(dst) == src):
            # Here either dst isn't a symlink or it's the wrong symlink.
            # Remove and recreate. Caller will just have to deal with any
            # exceptions at this stage.
            os.remove(dst)
            os.symlink(src, dst)

if __name__ == "__main__":
    main()
