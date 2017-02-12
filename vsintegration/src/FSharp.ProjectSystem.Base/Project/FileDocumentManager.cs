// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using Microsoft.VisualStudio.Shell.Interop;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Security;
using System.Windows;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using System.Collections;
using System.Text;
using IOleServiceProvider = Microsoft.VisualStudio.OLE.Interop.IServiceProvider;
using IServiceProvider = System.IServiceProvider;
using ShellConstants = Microsoft.VisualStudio.Shell.Interop.Constants;
using OleConstants = Microsoft.VisualStudio.OLE.Interop.Constants;

namespace Microsoft.VisualStudio.FSharp.ProjectSystem
{
    /// <summary>
    /// This class handles opening, saving of file items in the hierarchy.
    /// </summary>
    internal class FileDocumentManager : DocumentManager
    {
        public FileDocumentManager(FileNode node)
            : base(node)
        {
        }

        /// <summary>
        /// Open a file using the standard editor
        /// </summary>
        /// <param name="logicalView">In MultiView case determines view to be activated by IVsMultiViewDocumentView. For a list of logical view GUIDS, see constants starting with LOGVIEWID_ defined in NativeMethods class</param>
        /// <param name="docDataExisting">IntPtr to the IUnknown interface of the existing document data object</param>
        /// <param name="windowFrame">A reference to the window frame that is mapped to the file</param>
        /// <param name="windowFrameAction">Determine the UI action on the document window</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        public override int Open(ref Guid logicalView, IntPtr docDataExisting, out IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
        {
            bool newFile = false;
            bool openWith = false;
            return this.Open(newFile, openWith, ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
        }

        /// <summary>
        /// Open a file with a specific editor
        /// </summary>
        /// <param name="editorFlags">Specifies actions to take when opening a specific editor. Possible editor flags are defined in the enumeration Microsoft.VisualStudio.Shell.Interop.__VSOSPEFLAGS</param>
        /// <param name="editorType">Unique identifier of the editor type</param>
        /// <param name="physicalView">Name of the physical view. If null, the environment calls MapLogicalView on the editor factory to determine the physical view that corresponds to the logical view. In this case, null does not specify the primary view, but rather indicates that you do not know which view corresponds to the logical view</param>
        /// <param name="logicalView">In MultiView case determines view to be activated by IVsMultiViewDocumentView. For a list of logical view GUIDS, see constants starting with LOGVIEWID_ defined in NativeMethods class</param>
        /// <param name="docDataExisting">IntPtr to the IUnknown interface of the existing document data object</param>
        /// <param name="windowFrame">A reference to the window frame that is mapped to the file</param>
        /// <param name="windowFrameAction">Determine the UI action on the document window</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        public override int OpenWithSpecific(uint editorFlags, ref Guid editorType, string physicalView, ref Guid logicalView, IntPtr docDataExisting, out IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
        {
            windowFrame = null;
            bool newFile = false;
            bool openWith = false;
            return Open(newFile, openWith, editorFlags, ref editorType, physicalView, ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
        }

        /// <summary>
        /// Open a file with a specific editor
        /// </summary>
        /// <param name="editorFlags">Specifies actions to take when opening a specific editor. Possible editor flags are defined in the enumeration Microsoft.VisualStudio.Shell.Interop.__VSOSPEFLAGS</param>
        /// <param name="editorType">Unique identifier of the editor type</param>
        /// <param name="physicalView">Name of the physical view. If null, the environment calls MapLogicalView on the editor factory to determine the physical view that corresponds to the logical view. In this case, null does not specify the primary view, but rather indicates that you do not know which view corresponds to the logical view</param>
        /// <param name="logicalView">In MultiView case determines view to be activated by IVsMultiViewDocumentView. For a list of logical view GUIDS, see constants starting with LOGVIEWID_ defined in NativeMethods class</param>
        /// <param name="docDataExisting">IntPtr to the IUnknown interface of the existing document data object</param>
        /// <param name="windowFrame">A reference to the window frame that is mapped to the file</param>
        /// <param name="windowFrameAction">Determine the UI action on the document window</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        public override int ReOpenWithSpecific(uint editorFlags, ref Guid editorType, string physicalView, ref Guid logicalView, IntPtr docDataExisting, out IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
        {
            windowFrame = null;
            bool newFile = false;
            bool openWith = false;
            return Open(newFile, openWith, editorFlags, ref editorType, physicalView, ref logicalView, docDataExisting, out windowFrame, windowFrameAction, reopen: true);
        }

        /// <summary>
        /// Open a file in a document window with a std editor
        /// </summary>
        /// <param name="newFile">Open the file as a new file</param>
        /// <param name="openWith">Use a dialog box to determine which editor to use</param>
        /// <param name="windowFrameAction">Determine the UI action on the document window</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        public int Open(bool newFile, bool openWith, WindowFrameShowAction windowFrameAction)
        {
            var logicalView = Guid.Empty;
            IVsWindowFrame windowFrame = null;
            return this.Open(newFile, openWith, logicalView, out windowFrame, windowFrameAction);
        }

        /// <summary>
        /// Open a file in a document window with a std editor
        /// </summary>
        /// <param name="newFile">Open the file as a new file</param>
        /// <param name="openWith">Use a dialog box to determine which editor to use</param>
        /// <param name="logicalView">In MultiView case determines view to be activated by IVsMultiViewDocumentView. For a list of logical view GUIDS, see constants starting with LOGVIEWID_ defined in NativeMethods class</param>
        /// <param name="frame">A reference to the window frame that is mapped to the file</param>
        /// <param name="windowFrameAction">Determine the UI action on the document window</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        public int Open(bool newFile, bool openWith, Guid logicalView, out IVsWindowFrame frame, WindowFrameShowAction windowFrameAction)
        {
            frame = null;
            var rdt = this.Node.ProjectMgr.Site.GetService(typeof(SVsRunningDocumentTable)) as IVsRunningDocumentTable;
            Debug.Assert(rdt != null, " Could not get running document table from the services exposed by this project");

            if (rdt == null)
                return VSConstants.E_FAIL;

            // First we see if someone else has opened the requested view of the file.
            _VSRDTFLAGS flags = _VSRDTFLAGS.RDT_NoLock;
            uint itemid;
            var docData = IntPtr.Zero;
            IVsHierarchy ivsHierarchy;
            uint docCookie;
            var path = this.GetFullPathForDocument();
            var returnValue = VSConstants.S_OK;

            try
            {
                ErrorHandler.ThrowOnFailure(rdt.FindAndLockDocument((uint)flags, path, out ivsHierarchy, out itemid, out docData, out docCookie));
                ErrorHandler.ThrowOnFailure(this.Open(newFile, openWith, ref logicalView, docData, out frame, windowFrameAction));
            }
            catch (COMException e)
            {
                Trace.WriteLine("Exception :" + e.Message);
                returnValue = e.ErrorCode;
            }
            finally
            {
                if (docData != IntPtr.Zero)
                    Marshal.Release(docData);
            }

            return returnValue;
        }

        /// <summary>
        /// Open a file in a document window
        /// </summary>
        /// <param name="newFile">Open the file as a new file</param>
        /// <param name="openWith">Use a dialog box to determine which editor to use</param>
        /// <param name="logicalView">In MultiView case determines view to be activated by IVsMultiViewDocumentView. For a list of logical view GUIDS, see constants starting with LOGVIEWID_ defined in NativeMethods class</param>
        /// <param name="docDataExisting">IntPtr to the IUnknown interface of the existing document data object</param>
        /// <param name="windowFrame">A reference to the window frame that is mapped to the file</param>
        /// <param name="windowFrameAction">Determine the UI action on the document window</param>
        /// <returns>If the method succeeds, it returns S_OK. If it fails, it returns an error code.</returns>
        public virtual int Open(bool newFile, bool openWith, ref Guid logicalView, IntPtr docDataExisting, out IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction)
        {
            windowFrame = null;
            var editorType = Guid.Empty;
            return this.Open(newFile, openWith, 0, ref editorType, null, ref logicalView, docDataExisting, out windowFrame, windowFrameAction);
        }

        private int Open(bool newFile, bool openWith, uint editorFlags, ref Guid editorType, string physicalView, ref Guid logicalView, IntPtr docDataExisting, out IVsWindowFrame windowFrame, WindowFrameShowAction windowFrameAction, bool reopen = false)
        {
            windowFrame = null;
            Debug.Assert(this.Node != null, "No node has been initialized for the document manager");
            Debug.Assert(this.Node.ProjectMgr != null, "No project manager has been initialized for the document manager");
            Debug.Assert(this.Node is FileNode, "Node is not FileNode object");

            if (this.Node == null || this.Node.ProjectMgr == null || this.Node.ProjectMgr.IsClosed)
                return VSConstants.E_FAIL;

            var returnValue = VSConstants.S_OK;
            var caption = this.GetOwnerCaption();
            var fullPath = this.GetFullPathForDocument();

            var uiShellOpenDocument = this.Node.ProjectMgr.Site.GetService(typeof(SVsUIShellOpenDocument)) as IVsUIShellOpenDocument;
            var serviceProvider = this.Node.ProjectMgr.Site.GetService(typeof(IOleServiceProvider)) as IOleServiceProvider;

            var openState = uiShellOpenDocument as IVsUIShellOpenDocument3;
            bool showDialog = !reopen && (openState == null || !((__VSNEWDOCUMENTSTATE)openState.NewDocumentState).HasFlag(__VSNEWDOCUMENTSTATE.NDS_Provisional));

            // Make sure that the file is on disk before we open the editor and display message if not found
            if (!((FileNode)this.Node).IsFileOnDisk(showDialog))
            {
                // Bail since we are not able to open the item
                // Do not return an error code otherwise an internal error message is shown. The scenario for this operation
                // normally is already a reaction to a dialog box telling that the item has been removed.
                return VSConstants.S_FALSE;
            }

            try
            {
                this.Node.ProjectMgr.OnOpenItem(fullPath);
                var result = VSConstants.E_FAIL;

                if (openWith)
                {
                    result = uiShellOpenDocument.OpenStandardEditor((uint)__VSOSEFLAGS.OSE_UseOpenWithDialog, fullPath, ref logicalView, caption, this.Node.ProjectMgr, this.Node.ID, docDataExisting, serviceProvider, out windowFrame);
                }
                else
                {
                    __VSOSEFLAGS openFlags = 0;

                    if (newFile)
                        openFlags |= __VSOSEFLAGS.OSE_OpenAsNewFile;

                    //NOTE: we MUST pass the IVsProject in pVsUIHierarchy and the itemid
                    // of the node being opened, otherwise the debugger doesn't work.
                    if (editorType != Guid.Empty)
                    {
                        result = uiShellOpenDocument.OpenSpecificEditor(editorFlags, fullPath, ref editorType, physicalView, ref logicalView, caption, this.Node.ProjectMgr.InteropSafeIVsUIHierarchy, this.Node.ID, docDataExisting, serviceProvider, out windowFrame);
                    }
                    else
                    {
                        openFlags |= __VSOSEFLAGS.OSE_ChooseBestStdEditor;
                        result = uiShellOpenDocument.OpenStandardEditor((uint)openFlags, fullPath, ref logicalView, caption, this.Node.ProjectMgr, this.Node.ID, docDataExisting, serviceProvider, out windowFrame);
                    }
                }

                if (result != VSConstants.S_OK && result != VSConstants.S_FALSE && result != VSConstants.OLE_E_PROMPTSAVECANCELLED)
                    return result;

                if (windowFrame != null)
                {
                    object var;

                    if (newFile)
                    {
                        ErrorHandler.ThrowOnFailure(windowFrame.GetProperty((int)__VSFPROPID.VSFPROPID_DocData, out var));
                        IVsPersistDocData persistDocData = (IVsPersistDocData)var;
                        ErrorHandler.ThrowOnFailure(persistDocData.SetUntitledDocPath(fullPath));
                    }

                    var = null;
                    ErrorHandler.ThrowOnFailure(windowFrame.GetProperty((int)__VSFPROPID.VSFPROPID_DocCookie, out var));
                    this.Node.DocCookie = (uint)(int)var;

                    if (windowFrameAction == WindowFrameShowAction.Show)
                        ErrorHandler.ThrowOnFailure(windowFrame.Show());
                    else if (windowFrameAction == WindowFrameShowAction.ShowNoActivate)
                        ErrorHandler.ThrowOnFailure(windowFrame.ShowNoActivate());
                    else if (windowFrameAction == WindowFrameShowAction.Hide)
                        ErrorHandler.ThrowOnFailure(windowFrame.Hide());
                }
            }
            catch (COMException e)
            {
                Trace.WriteLine("Exception e:" + e.Message);
                returnValue = e.ErrorCode;
                CloseWindowFrame(ref windowFrame);
            }

            return returnValue;
        }

        private new FileNode Node => (FileNode)base.Node;
    }
}