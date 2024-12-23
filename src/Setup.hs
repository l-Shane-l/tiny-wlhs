module Setup where

import Foreign.Ptr
import qualified LibTinyWLHS.Compositor.FFI as FFI
import qualified LibTinyWLHS.Compositor.Types as FFI
import qualified LibTinyWLHS.Server.FFI as FFI
import qualified LibTinyWLHS.Server.Server as Server
import qualified LibTinyWLHS.Server.Types as FFI
import WLR.Util.Log

setupServer :: IO (Maybe (Ptr FFI.TinyWLServer, Ptr FFI.WlrRenderer))
setupServer = do
    server <- FFI.c_server_create
    wlr_log WLR_DEBUG "Server created"

    display <- FFI.c_wl_display_create
    wlr_log WLR_DEBUG "Display created"

    _ <- Server.setWlDisplay server display
    wlr_log WLR_DEBUG "Set WLDisplay"

    backend <- FFI.c_wlr_backend_autocreate display nullPtr
    wlr_log WLR_DEBUG "Backend autocreate"

    _ <- Server.setBackend server backend
    wlr_log WLR_DEBUG "Set Backend"

    renderer <- FFI.c_wlr_renderer_autocreate backend
    wlr_log WLR_DEBUG "create renderer with autocreate"

    _ <- Server.setRenderer server renderer
    wlr_log WLR_DEBUG "Set renderer"

    _ <- FFI.c_wlr_renderer_init_display renderer display
    wlr_log WLR_DEBUG "render init display"

    allocator <- FFI.c_wlr_allocator_autocreate backend renderer
    wlr_log WLR_DEBUG "autocrate allocator"

    _ <- Server.setAllocator server allocator
    wlr_log WLR_DEBUG "Set allocator"

    outputLayout <- FFI.c_wlr_output_layout_create
    wlr_log WLR_DEBUG $ "create output layout " ++ show outputLayout

    if outputLayout == nullPtr
        then do
            wlr_log WLR_ERROR "Failed to create output layout"
        else do
            wlr_log WLR_INFO $ "About to set outputLayout pointer: " ++ show outputLayout
            _ <- Server.setOutputLayout server outputLayout
            wlr_log WLR_INFO "Output layout set attempt completed"

    layoutPtr <- Server.getOutputLayout server
    wlr_log WLR_INFO $ "Retrieved layoutPtr: " ++ show layoutPtr

    if layoutPtr == nullPtr
        then do
            wlr_log WLR_ERROR "Output layout is null after setting"
            error "Critical: Output layout is null after setting"
        else
            wlr_log WLR_INFO "Final output layout pointer: "

    pure $ Just (server, renderer)

commandLineArguments :: [String] -> IO ()
commandLineArguments args = do
    case args of
        ("-h" : _) -> do
            wlr_log WLR_INFO "Please read the README for instructions on using this app"
        _ -> do
            wlr_log WLR_INFO "Command line arguments no longer supported, please configure app in Config.hs"
