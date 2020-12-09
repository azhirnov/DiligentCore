/*
 *  Copyright 2019-2020 Diligent Graphics LLC
 *  Copyright 2015-2019 Egor Yusov
 *  
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *      http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *  In no event and under no legal theory, whether in tort (including negligence), 
 *  contract, or otherwise, unless required by applicable law (such as deliberate 
 *  and grossly negligent acts) or agreed to in writing, shall any Contributor be
 *  liable for any damages, including any direct, indirect, special, incidental, 
 *  or consequential damages of any character arising as a result of this License or 
 *  out of the use or inability to use the software (including but not limited to damages 
 *  for loss of goodwill, work stoppage, computer failure or malfunction, or any and 
 *  all other commercial damages or losses), even if such Contributor has been advised 
 *  of the possibility of such damages.
 */

#include "DiligentCore/Graphics/GraphicsEngine/interface/DeviceContext.h"

void TestDeviceContextCInterface(struct IDeviceContext* pCtx)
{
    struct IPipelineState*            pPSO                       = NULL;
    struct DrawAttribs                drawAttribs                = {0};
    struct DrawIndexedAttribs         drawIndexedAttribs         = {0};
    struct DrawIndirectAttribs        drawIndirectAttribs        = {0};
    struct DrawIndexedIndirectAttribs drawIndexedIndirectAttribs = {0};
    struct IBuffer*                   pIndirectBuffer            = NULL;

    IDeviceContext_SetPipelineState(pCtx, pPSO);
    IDeviceContext_Draw(pCtx, &drawAttribs);
    IDeviceContext_DrawIndexed(pCtx, &drawIndexedAttribs);
    IDeviceContext_DrawIndirect(pCtx, &drawIndirectAttribs, pIndirectBuffer);
    IDeviceContext_DrawIndexedIndirect(pCtx, &drawIndexedIndirectAttribs, pIndirectBuffer);

    struct DrawMeshAttribs drawMeshAttribs = {0};
    IDeviceContext_DrawMesh(pCtx, &drawMeshAttribs);

    struct DrawMeshIndirectAttribs drawMeshIndirectAttribs = {0};
    IDeviceContext_DrawMeshIndirect(pCtx, &drawMeshIndirectAttribs, pIndirectBuffer);

    struct BuildBLASAttribs buildBLASAttribs = {0};
    IDeviceContext_BuildBLAS(pCtx, &buildBLASAttribs);

    struct BuildTLASAttribs buildTLASAttribs = {0};
    IDeviceContext_BuildTLAS(pCtx, &buildTLASAttribs);

    struct CopyBLASAttribs copyBLASAttribs = {0};
    IDeviceContext_CopyBLAS(pCtx, &copyBLASAttribs);

    struct CopyTLASAttribs copyTLASAttribs = {0};
    IDeviceContext_CopyTLAS(pCtx, &copyTLASAttribs);

    struct WriteBLASCompactedSizeAttribs writeBLASCompactedSizeAttribs = {0};
    IDeviceContext_WriteBLASCompactedSize(pCtx, &writeBLASCompactedSizeAttribs);

    struct WriteTLASCompactedSizeAttribs writeTLASCompactedSizeAttribs = {0};
    IDeviceContext_WriteTLASCompactedSize(pCtx, &writeTLASCompactedSizeAttribs);

    struct TraceRaysAttribs traceRaysAttribs = {0};
    IDeviceContext_TraceRays(pCtx, &traceRaysAttribs);

    struct TraceRaysIndirectAttribs traceRaysIndirectAttribs = {0};
    IDeviceContext_TraceRaysIndirect(pCtx, &traceRaysIndirectAttribs, pIndirectBuffer);
}
