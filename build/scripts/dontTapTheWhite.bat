@rem
@rem Copyright 2015 the original author or authors.
@rem
@rem Licensed under the Apache License, Version 2.0 (the "License");
@rem you may not use this file except in compliance with the License.
@rem You may obtain a copy of the License at
@rem
@rem      https://www.apache.org/licenses/LICENSE-2.0
@rem
@rem Unless required by applicable law or agreed to in writing, software
@rem distributed under the License is distributed on an "AS IS" BASIS,
@rem WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
@rem See the License for the specific language governing permissions and
@rem limitations under the License.
@rem
@rem SPDX-License-Identifier: Apache-2.0
@rem

@if "%DEBUG%"=="" @echo off
@rem ##########################################################################
@rem
@rem  dontTapTheWhite startup script for Windows
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

set DIRNAME=%~dp0
if "%DIRNAME%"=="" set DIRNAME=.
@rem This is normally unused
set APP_BASE_NAME=%~n0
set APP_HOME=%DIRNAME%..

@rem Resolve any "." and ".." in APP_HOME to make it shorter.
for %%i in ("%APP_HOME%") do set APP_HOME=%%~fi

@rem Add default JVM options here. You can also use JAVA_OPTS and DONT_TAP_THE_WHITE_OPTS to pass JVM options to this script.
set DEFAULT_JVM_OPTS=

@rem Find java.exe
if defined JAVA_HOME goto findJavaFromJavaHome

set JAVA_EXE=java.exe
%JAVA_EXE% -version >NUL 2>&1
if %ERRORLEVEL% equ 0 goto execute

echo. 1>&2
echo ERROR: JAVA_HOME is not set and no 'java' command could be found in your PATH. 1>&2
echo. 1>&2
echo Please set the JAVA_HOME variable in your environment to match the 1>&2
echo location of your Java installation. 1>&2

goto fail

:findJavaFromJavaHome
set JAVA_HOME=%JAVA_HOME:"=%
set JAVA_EXE=%JAVA_HOME%/bin/java.exe

if exist "%JAVA_EXE%" goto execute

echo. 1>&2
echo ERROR: JAVA_HOME is set to an invalid directory: %JAVA_HOME% 1>&2
echo. 1>&2
echo Please set the JAVA_HOME variable in your environment to match the 1>&2
echo location of your Java installation. 1>&2

goto fail

:execute
@rem Setup the command line

set CLASSPATH=%APP_HOME%\lib\dontTapTheWhite.jar;%APP_HOME%\lib\processing-core-4-4.3.jar;%APP_HOME%\lib\gluegen-rt-main-2.5.0-rc-20230523.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-android-aarch64.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-linux-amd64.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-linux-armv6hf.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-linux-aarch64.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-macosx-universal.jar;%APP_HOME%\lib\gluegen-rt-2.5.0-rc-20230523-natives-windows-amd64.jar;%APP_HOME%\lib\scala-library-2.13.14.jar;%APP_HOME%\lib\minim-2.2.2.jar;%APP_HOME%\lib\javafx-controls-19.0.2.1.jar;%APP_HOME%\lib\jogl-all-main-2.5.0-rc-20230523.jar;%APP_HOME%\lib\batik-svggen-1.16.jar;%APP_HOME%\lib\batik-dom-1.16.jar;%APP_HOME%\lib\itext-2.1.7.jar;%APP_HOME%\lib\maven-javadoc-plugin-2.10.4.jar;%APP_HOME%\lib\doxia-site-renderer-1.4.jar;%APP_HOME%\lib\velocity-tools-2.0.jar;%APP_HOME%\lib\struts-taglib-1.3.8.jar;%APP_HOME%\lib\struts-tiles-1.3.8.jar;%APP_HOME%\lib\struts-core-1.3.8.jar;%APP_HOME%\lib\antlr-2.7.7.jar;%APP_HOME%\lib\mp3spi-1.9.5-1.jar;%APP_HOME%\lib\tritonus-share-0.3.7-2.jar;%APP_HOME%\lib\jlayer-1.0.1.jar;%APP_HOME%\lib\javafx-graphics-19.0.2.1.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-android-aarch64.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-linux-amd64.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-linux-armv6hf.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-linux-aarch64.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-macosx-universal.jar;%APP_HOME%\lib\jogl-all-2.5.0-rc-20230523-natives-windows-amd64.jar;%APP_HOME%\lib\batik-awt-util-1.16.jar;%APP_HOME%\lib\batik-css-1.16.jar;%APP_HOME%\lib\batik-xml-1.16.jar;%APP_HOME%\lib\batik-util-1.16.jar;%APP_HOME%\lib\batik-ext-1.16.jar;%APP_HOME%\lib\batik-constants-1.16.jar;%APP_HOME%\lib\batik-i18n-1.16.jar;%APP_HOME%\lib\batik-shared-resources-1.16.jar;%APP_HOME%\lib\xml-apis-ext-1.3.04.jar;%APP_HOME%\lib\bcmail-jdk14-138.jar;%APP_HOME%\lib\bcprov-jdk14-138.jar;%APP_HOME%\lib\maven-toolchain-2.2.1.jar;%APP_HOME%\lib\maven-archiver-2.5.jar;%APP_HOME%\lib\maven-core-2.2.1.jar;%APP_HOME%\lib\maven-common-artifact-filters-1.3.jar;%APP_HOME%\lib\maven-project-2.2.1.jar;%APP_HOME%\lib\maven-settings-2.2.1.jar;%APP_HOME%\lib\maven-profile-2.2.1.jar;%APP_HOME%\lib\maven-model-2.2.1.jar;%APP_HOME%\lib\maven-plugin-descriptor-2.2.1.jar;%APP_HOME%\lib\maven-plugin-api-2.2.1.jar;%APP_HOME%\lib\maven-artifact-manager-2.2.1.jar;%APP_HOME%\lib\maven-artifact-2.2.1.jar;%APP_HOME%\lib\maven-reporting-api-3.0.jar;%APP_HOME%\lib\maven-invoker-2.2.jar;%APP_HOME%\lib\doxia-module-xhtml-1.4.jar;%APP_HOME%\lib\doxia-module-fml-1.4.jar;%APP_HOME%\lib\doxia-core-1.4.jar;%APP_HOME%\lib\doxia-sink-api-1.4.jar;%APP_HOME%\lib\wagon-file-1.0-beta-6.jar;%APP_HOME%\lib\wagon-http-lightweight-1.0-beta-6.jar;%APP_HOME%\lib\wagon-http-1.0-beta-6.jar;%APP_HOME%\lib\wagon-webdav-jackrabbit-1.0-beta-6.jar;%APP_HOME%\lib\wagon-ssh-external-1.0-beta-6.jar;%APP_HOME%\lib\wagon-ssh-1.0-beta-6.jar;%APP_HOME%\lib\wagon-http-shared-1.0-beta-6.jar;%APP_HOME%\lib\wagon-ssh-common-1.0-beta-6.jar;%APP_HOME%\lib\wagon-provider-api-1.0-beta-6.jar;%APP_HOME%\lib\velocity-1.6.2.jar;%APP_HOME%\lib\commons-lang-2.4.jar;%APP_HOME%\lib\plexus-archiver-3.3.jar;%APP_HOME%\lib\xmlgraphics-commons-2.7.jar;%APP_HOME%\lib\plexus-io-2.7.1.jar;%APP_HOME%\lib\commons-io-2.5.jar;%APP_HOME%\lib\httpclient-4.2.3.jar;%APP_HOME%\lib\commons-chain-1.1.jar;%APP_HOME%\lib\commons-validator-1.3.1.jar;%APP_HOME%\lib\commons-digester-1.8.jar;%APP_HOME%\lib\commons-beanutils-1.7.0.jar;%APP_HOME%\lib\jackrabbit-webdav-1.5.0.jar;%APP_HOME%\lib\commons-httpclient-3.1.jar;%APP_HOME%\lib\commons-logging-1.1.1.jar;%APP_HOME%\lib\log4j-1.2.14.jar;%APP_HOME%\lib\qdox-1.12.1.jar;%APP_HOME%\lib\maven-plugin-parameter-documenter-2.2.1.jar;%APP_HOME%\lib\maven-error-diagnostics-2.2.1.jar;%APP_HOME%\lib\maven-plugin-registry-2.2.1.jar;%APP_HOME%\lib\doxia-logging-api-1.4.jar;%APP_HOME%\lib\plexus-velocity-1.1.7.jar;%APP_HOME%\lib\plexus-container-default-1.0-alpha-30.jar;%APP_HOME%\lib\maven-repository-metadata-2.2.1.jar;%APP_HOME%\lib\plexus-sec-dispatcher-1.3.jar;%APP_HOME%\lib\doxia-decoration-model-1.4.jar;%APP_HOME%\lib\plexus-i18n-1.0-beta-7.jar;%APP_HOME%\lib\plexus-interactivity-api-1.0-alpha-6.jar;%APP_HOME%\lib\plexus-utils-3.0.24.jar;%APP_HOME%\lib\jlayer-1.0.1-1.jar;%APP_HOME%\lib\javafx-base-19.0.2.1.jar;%APP_HOME%\lib\bctsp-jdk14-1.38.jar;%APP_HOME%\lib\slf4j-jdk14-1.5.6.jar;%APP_HOME%\lib\jcl-over-slf4j-1.5.6.jar;%APP_HOME%\lib\commons-cli-1.2.jar;%APP_HOME%\lib\maven-monitor-2.2.1.jar;%APP_HOME%\lib\classworlds-1.1.jar;%APP_HOME%\lib\plexus-interpolation-1.15.jar;%APP_HOME%\lib\backport-util-concurrent-3.1.jar;%APP_HOME%\lib\plexus-component-annotations-1.6.jar;%APP_HOME%\lib\commons-collections-3.2.1.jar;%APP_HOME%\lib\httpcore-4.2.2.jar;%APP_HOME%\lib\commons-codec-1.6.jar;%APP_HOME%\lib\commons-compress-1.11.jar;%APP_HOME%\lib\snappy-0.4.jar;%APP_HOME%\lib\xz-1.5.jar;%APP_HOME%\lib\plexus-component-api-1.0-alpha-16.jar;%APP_HOME%\lib\plexus-classworlds-1.2-alpha-9.jar;%APP_HOME%\lib\junit-3.8.2.jar;%APP_HOME%\lib\bcmail-jdk14-1.38.jar;%APP_HOME%\lib\bcprov-jdk14-1.38.jar;%APP_HOME%\lib\slf4j-nop-1.5.3.jar;%APP_HOME%\lib\slf4j-api-1.5.6.jar;%APP_HOME%\lib\jsch-0.1.38.jar;%APP_HOME%\lib\plexus-cipher-1.4.jar;%APP_HOME%\lib\xercesImpl-2.9.1.jar;%APP_HOME%\lib\oro-2.0.8.jar;%APP_HOME%\lib\dom4j-1.1.jar;%APP_HOME%\lib\sslext-1.2-0.jar;%APP_HOME%\lib\xercesMinimal-1.9.6.2.jar;%APP_HOME%\lib\nekohtml-1.9.6.2.jar;%APP_HOME%\lib\jackrabbit-jcr-commons-1.5.0.jar;%APP_HOME%\lib\xml-apis-1.3.04.jar


@rem Execute dontTapTheWhite
"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %JAVA_OPTS% %DONT_TAP_THE_WHITE_OPTS%  -classpath "%CLASSPATH%" tetris.game.TetrisGame %*

:end
@rem End local scope for the variables with windows NT shell
if %ERRORLEVEL% equ 0 goto mainEnd

:fail
rem Set variable DONT_TAP_THE_WHITE_EXIT_CONSOLE if you need the _script_ return code instead of
rem the _cmd.exe /c_ return code!
set EXIT_CODE=%ERRORLEVEL%
if %EXIT_CODE% equ 0 set EXIT_CODE=1
if not ""=="%DONT_TAP_THE_WHITE_EXIT_CONSOLE%" exit %EXIT_CODE%
exit /b %EXIT_CODE%

:mainEnd
if "%OS%"=="Windows_NT" endlocal

:omega
