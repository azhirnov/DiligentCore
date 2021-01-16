
#include <android_native_app_glue.h>
#include "GraphicsTypes.h"
#include "EngineFactoryVk.h"
#include "RefCntAutoPtr.hpp"

using namespace Diligent;

struct AndroidWnd
{
    ANativeWindow* window   = nullptr;
    int            window_w = 0;
    int            window_h = 0;
    bool           pause    = false;

    RefCntAutoPtr<IDeviceContext> m_pContext;
    RefCntAutoPtr<IRenderDevice>  m_pDevice;
    RefCntAutoPtr<ISwapChain>     m_pSwapChain;
    RefCntAutoPtr<IEngineFactory> m_pEngineFactory;
};

static void HandleCmd(android_app* app, int32_t cmd)
{
    AndroidWnd* self = static_cast<AndroidWnd *>(app->userData);
    switch (cmd)
    {
        case APP_CMD_SAVE_STATE :
            break;

        case APP_CMD_INIT_WINDOW :
            if (self->window != nullptr)
            {
                self->window   = app->window;
                self->window_w = ANativeWindow_getWidth(self->window);
                self->window_h = ANativeWindow_getHeight(self->window);

                // create device
                {
                    EngineVkCreateInfo CreateInfo;
                    CreateInfo.EnableValidation    = true;
                    CreateInfo.NumDeferredContexts = 0;
                    CreateInfo.Features.RayTracing = DEVICE_FEATURE_STATE_ENABLED;

                    auto* Factory    = GetEngineFactoryVk();
                    self->m_pEngineFactory = Factory;

                    IRenderDevice*  Device  = nullptr;
                    IDeviceContext* Context = nullptr;
                    Factory->CreateDeviceAndContextsVk(CreateInfo, &Device, &Context);
                    if (!Device)
                        return;

                    self->m_pDevice  = Device;
                    self->m_pContext = Context;

                    SwapChainDesc       SCDesc;
                    AndroidNativeWindow Window{app->window};
                    Factory->CreateSwapChainVk(self->m_pDevice, self->m_pContext, SCDesc, Window, &self->m_pSwapChain);

                    if (self->m_pSwapChain == nullptr)
                    {
                        LOG_ERROR("Failed to create swapchain for VR emulator");
                        return;
                    }
                }
            }
            break;

        case APP_CMD_TERM_WINDOW :
            self->window = nullptr;
            break;

        case APP_CMD_GAINED_FOCUS :
            self->pause = false;
            break;

        case APP_CMD_LOST_FOCUS :
            self->pause = true;
            break;
    }
}

void android_main (struct android_app* app)
{
    AndroidWnd           wnd      = {};
    int                  ident;
    int                  events;
    android_poll_source* source   = nullptr;

    app->userData = &wnd;
    app->onAppCmd = &HandleCmd;

    while (true)
    {
        if ((ident = ALooper_pollAll(0, nullptr, &events, (void**)&source)) >= 0)
        {
            if (source != nullptr)
                source->process(app, source);

            if (app->destroyRequested)
                break;
        }
    }
}
