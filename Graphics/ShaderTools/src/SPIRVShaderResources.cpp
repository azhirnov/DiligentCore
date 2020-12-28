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

#include <iomanip>
#include "SPIRVShaderResources.hpp"
#include "spirv_reflect.h"
#include "include/spirv/unified1/spirv.hpp"
#include "ShaderBase.hpp"
#include "GraphicsAccessories.hpp"
#include "StringTools.hpp"
#include "Align.hpp"

namespace Diligent
{

template <typename Type>
Type GetResourceArraySize(const SpvReflectDescriptorBinding& Binding)
{
    uint32_t arrSize = 1;
    if (Binding.array.dims_count)
    {
        VERIFY(Binding.array.dims_count == 1, "Only one-dimensional arrays are currently supported");
        arrSize = Binding.array.dims[0];
    }
    VERIFY(arrSize <= std::numeric_limits<Type>::max(), "Array size exceeds maximum representable value ", std::numeric_limits<Type>::max());
    return static_cast<Type>(arrSize);
}

static RESOURCE_DIMENSION GetResourceDimension(const SpvReflectDescriptorBinding& Binding)
{
    if (Binding.descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLED_IMAGE ||
        Binding.descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER ||
        Binding.descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_IMAGE)
    {
        switch (Binding.image.dim)
        {
            // clang-format off
            case spv::Dim1D:     return Binding.image.arrayed ? RESOURCE_DIM_TEX_1D_ARRAY : RESOURCE_DIM_TEX_1D;
            case spv::Dim2D:     return Binding.image.arrayed ? RESOURCE_DIM_TEX_2D_ARRAY : RESOURCE_DIM_TEX_2D;
            case spv::Dim3D:     return RESOURCE_DIM_TEX_3D;
            case spv::DimCube:   return Binding.image.arrayed ? RESOURCE_DIM_TEX_CUBE_ARRAY : RESOURCE_DIM_TEX_CUBE;
            case spv::DimBuffer: return RESOURCE_DIM_BUFFER;
            // clang-format on
            default: return RESOURCE_DIM_UNDEFINED;
        }
    }
    else
    {
        return RESOURCE_DIM_UNDEFINED;
    }
}

static bool IsMultisample(const SpvReflectDescriptorBinding& Binding)
{
    if (Binding.descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLED_IMAGE ||
        Binding.descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER ||
        Binding.descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_IMAGE ||
        Binding.descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_INPUT_ATTACHMENT)
    {
        return Binding.image.ms;
    }
    else
    {
        return RESOURCE_DIM_UNDEFINED;
    }
}

SPIRVShaderResourceAttribs::SPIRVShaderResourceAttribs(const SpvReflectDescriptorBinding& Binding,
                                                       const char*                        _Name,
                                                       ResourceType                       _Type,
                                                       Uint32                             _SepSmplrOrImgInd,
                                                       Uint32                             _BufferStaticSize,
                                                       Uint32                             _BufferStride) noexcept :
    // clang-format off
    Name                          {_Name},
    ArraySize                     {GetResourceArraySize<decltype(ArraySize)>(Binding)},
    Type                          {_Type},
    ResourceDim                   {Diligent::GetResourceDimension(Binding)},
    IsMS                          {Diligent::IsMultisample(Binding) ? Uint8{1} : Uint8{0}},
    SepSmplrOrImgInd              {_SepSmplrOrImgInd},
    BindingDecorationOffset       {Binding.word_offset.binding},
    DescriptorSetDecorationOffset {Binding.word_offset.set},
    BufferStaticSize              {_BufferStaticSize},
    BufferStride                  {_BufferStride}
// clang-format on
{
    VERIFY(_SepSmplrOrImgInd == SPIRVShaderResourceAttribs::InvalidSepSmplrOrImgInd ||
               (_Type == ResourceType::SeparateSampler || _Type == ResourceType::SeparateImage),
           "Only separate images or separate samplers can be assinged valid SepSmplrOrImgInd value");
}

SHADER_RESOURCE_TYPE SPIRVShaderResourceAttribs::GetShaderResourceType(ResourceType Type)
{
    static_assert(Uint32{SPIRVShaderResourceAttribs::ResourceType::NumResourceTypes} == 12, "Please handle the new resource type below");
    switch (Type)
    {
        case SPIRVShaderResourceAttribs::ResourceType::UniformBuffer:
            return SHADER_RESOURCE_TYPE_CONSTANT_BUFFER;

        case SPIRVShaderResourceAttribs::ResourceType::ROStorageBuffer:
            // Read-only storage buffers map to buffer SRV
            // https://github.com/KhronosGroup/SPIRV-Cross/wiki/Reflection-API-user-guide#read-write-vs-read-only-resources-for-hlsl
            return SHADER_RESOURCE_TYPE_BUFFER_SRV;

        case SPIRVShaderResourceAttribs::ResourceType::RWStorageBuffer:
            return SHADER_RESOURCE_TYPE_BUFFER_UAV;

        case SPIRVShaderResourceAttribs::ResourceType::UniformTexelBuffer:
            return SHADER_RESOURCE_TYPE_BUFFER_SRV;

        case SPIRVShaderResourceAttribs::ResourceType::StorageTexelBuffer:
            return SHADER_RESOURCE_TYPE_BUFFER_UAV;

        case SPIRVShaderResourceAttribs::ResourceType::StorageImage:
            return SHADER_RESOURCE_TYPE_TEXTURE_UAV;

        case SPIRVShaderResourceAttribs::ResourceType::SampledImage:
            return SHADER_RESOURCE_TYPE_TEXTURE_SRV;

        case SPIRVShaderResourceAttribs::ResourceType::AtomicCounter:
            LOG_WARNING_MESSAGE("There is no appropriate shader resource type for atomic counter");
            return SHADER_RESOURCE_TYPE_BUFFER_UAV;

        case SPIRVShaderResourceAttribs::ResourceType::SeparateImage:
            return SHADER_RESOURCE_TYPE_TEXTURE_SRV;

        case SPIRVShaderResourceAttribs::ResourceType::SeparateSampler:
            return SHADER_RESOURCE_TYPE_SAMPLER;

        case SPIRVShaderResourceAttribs::ResourceType::InputAttachment:
            return SHADER_RESOURCE_TYPE_INPUT_ATTACHMENT;

        case SPIRVShaderResourceAttribs::ResourceType::AccelerationStructure:
            return SHADER_RESOURCE_TYPE_ACCEL_STRUCT;

        default:
            UNEXPECTED("Unknown SPIRV resource type");
            return SHADER_RESOURCE_TYPE_UNKNOWN;
    }
}


static spv::ExecutionModel ShaderTypeToExecutionModel(SHADER_TYPE ShaderType)
{
    static_assert(SHADER_TYPE_LAST == SHADER_TYPE_CALLABLE, "Please handle the new shader type in the switch below");
    switch (ShaderType)
    {
        // clang-format off
        case SHADER_TYPE_VERTEX:           return spv::ExecutionModelVertex;
        case SHADER_TYPE_HULL:             return spv::ExecutionModelTessellationControl;
        case SHADER_TYPE_DOMAIN:           return spv::ExecutionModelTessellationEvaluation;
        case SHADER_TYPE_GEOMETRY:         return spv::ExecutionModelGeometry;
        case SHADER_TYPE_PIXEL:            return spv::ExecutionModelFragment;
        case SHADER_TYPE_COMPUTE:          return spv::ExecutionModelGLCompute;
        case SHADER_TYPE_AMPLIFICATION:    return spv::ExecutionModelTaskNV;
        case SHADER_TYPE_MESH:             return spv::ExecutionModelMeshNV;
        case SHADER_TYPE_RAY_GEN:          return spv::ExecutionModelRayGenerationKHR;
        case SHADER_TYPE_RAY_MISS:         return spv::ExecutionModelMissKHR;
        case SHADER_TYPE_RAY_CLOSEST_HIT:  return spv::ExecutionModelClosestHitKHR;
        case SHADER_TYPE_RAY_ANY_HIT:      return spv::ExecutionModelAnyHitKHR;
        case SHADER_TYPE_RAY_INTERSECTION: return spv::ExecutionModelIntersectionKHR;
        case SHADER_TYPE_CALLABLE:         return spv::ExecutionModelCallableKHR;
        // clang-format on
        default:
            UNEXPECTED("Unexpected shader type");
            return spv::ExecutionModelVertex;
    }
}

static const char* GetResourceName(const SpvReflectDescriptorBinding* binding, bool isHLSL)
{
    if (binding->type_description != nullptr && binding->type_description->type_name != nullptr)
    {
        if (binding->name == nullptr || binding->name[0] == 0)
            isHLSL = false;

        switch (binding->descriptor_type)
        {
            case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
            case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:
                return isHLSL ? binding->name : binding->type_description->type_name;

            case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER:
            case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC:
                return isHLSL ? binding->name : binding->type_description->type_name;
        }
    }

    return binding->name;
}

SPIRVShaderResources::SPIRVShaderResources(IMemoryAllocator&     Allocator,
                                           IRenderDevice*        pRenderDevice,
                                           std::vector<uint32_t> spirv_binary,
                                           const ShaderDesc&     shaderDesc,
                                           const char*           CombinedSamplerSuffix,
                                           bool                  LoadShaderStageInputs,
                                           std::string&          EntryPoint) :
    m_ShaderType{shaderDesc.ShaderType}
{
    SpvReflectShaderModule Module = {};
    if (spvReflectCreateShaderModule(spirv_binary.size() * sizeof(spirv_binary[0]), spirv_binary.data(), &Module) != SPV_REFLECT_RESULT_SUCCESS)
        return;

    spv::ExecutionModel   ExecutionModel = ShaderTypeToExecutionModel(shaderDesc.ShaderType);
    SpvReflectEntryPoint* pReflection    = nullptr;

    m_IsHLSLSource = (Module.source_language == SpvSourceLanguage::SpvSourceLanguageHLSL);

    for (uint32_t i = 0; i < Module.entry_point_count; ++i)
    {
        if (Module.entry_points[i].spirv_execution_model == ExecutionModel)
        {
            if (!EntryPoint.empty())
            {
                LOG_WARNING_MESSAGE("More than one entry point of type ", GetShaderTypeLiteralName(shaderDesc.ShaderType), " found in SPIRV binary for shader '", shaderDesc.Name, "'. The first one ('", EntryPoint, "') will be used.");
            }
            else
            {
                EntryPoint  = Module.entry_points[i].name;
                pReflection = &Module.entry_points[i];
            }
        }
    }
    if (EntryPoint.empty() || pReflection == nullptr)
    {
        LOG_ERROR_AND_THROW("Unable to find entry point of type ", GetShaderTypeLiteralName(shaderDesc.ShaderType), " in SPIRV binary for shader '", shaderDesc.Name, "'");
    }

    if (pReflection->used_push_constant_count != 0)
    {
        LOG_ERROR_AND_THROW("Push constants is not supported, in SPIRV binary for shader '", shaderDesc.Name, "'");
    }

    ResourceCounters ResCounters;
    size_t           ResourceNamesPoolSize = 0;

    for (uint32_t ds = 0; ds < pReflection->descriptor_set_count; ++ds)
    {
        const SpvReflectDescriptorSet& descSet = pReflection->descriptor_sets[ds];
        for (uint32_t b = 0; b < descSet.binding_count; ++b)
        {
            const SpvReflectDescriptorBinding* binding = descSet.bindings[b];
            if (binding == nullptr)
                continue;

            const char* name = GetResourceName(binding, m_IsHLSLSource);
            ResourceNamesPoolSize += strlen(name) + 1;

            static_assert(Uint32{SPIRVShaderResourceAttribs::ResourceType::NumResourceTypes} == 12, "Please set the new resource type in switch below");
            switch (binding->descriptor_type)
            {
                // clang-format off
                case SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLER:                    ++ResCounters.NumSepSmplrs;    break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLED_IMAGE:              ++ResCounters.NumSepImgs;      break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER:     ++ResCounters.NumSmpldImgs;    break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_IMAGE:              ++ResCounters.NumImgs;         break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER:       ++ResCounters.NumSmpldImgs;    break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER:       ++ResCounters.NumImgs;         break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
                case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:     ++ResCounters.NumUBs;          break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER:
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC:     ++ResCounters.NumSBs;          break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_INPUT_ATTACHMENT:           ++ResCounters.NumInptAtts;     break;
                case SPV_REFLECT_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR: ++ResCounters.NumAccelStructs; break;
                    // clang-format on
            }
        }
    }
    if (CombinedSamplerSuffix != nullptr)
    {
        ResourceNamesPoolSize += strlen(CombinedSamplerSuffix) + 1;
    }

    VERIFY_EXPR(shaderDesc.Name != nullptr);
    ResourceNamesPoolSize += strlen(shaderDesc.Name) + 1;

    Uint32 NumShaderStageInputs = 0;

    if (!m_IsHLSLSource || Module.interface_variable_count == 0)
        LoadShaderStageInputs = false;

    if (LoadShaderStageInputs)
    {
        for (uint32_t i = 0; i < pReflection->input_variable_count; ++i)
        {
            const SpvReflectInterfaceVariable* input = pReflection->input_variables[i];
            if (input == nullptr || input->built_in != -1)
                continue;

            ResourceNamesPoolSize += strlen(input->semantic) + 1;
            ++NumShaderStageInputs;
        }
    }

    // Resource names pool is only needed to facilitate string allocation.
    StringPool ResourceNamesPool;
    Initialize(Allocator, ResCounters, NumShaderStageInputs, ResourceNamesPoolSize, ResourceNamesPool);

    ResourceCounters Counters;
    for (uint32_t ds = 0; ds < pReflection->descriptor_set_count; ++ds)
    {
        const SpvReflectDescriptorSet& descSet = pReflection->descriptor_sets[ds];
        for (uint32_t b = 0; b < descSet.binding_count; ++b)
        {
            const SpvReflectDescriptorBinding* binding = descSet.bindings[b];
            if (binding == nullptr)
                continue;

            static_assert(Uint32{SPIRVShaderResourceAttribs::ResourceType::NumResourceTypes} == 12, "Please set the new resource type in switch below");
            switch (binding->descriptor_type)
            {
                case SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLER:
                {
                    new (&GetSepSmplr(Counters.NumSepSmplrs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::SeparateSampler);
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER:
                {
                    new (&GetSmpldImg(Counters.NumSmpldImgs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::SampledImage);
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_IMAGE:
                {
                    new (&GetImg(Counters.NumImgs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::StorageImage);
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER:
                {
                    new (&GetSmpldImg(Counters.NumSmpldImgs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::UniformTexelBuffer);
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER:
                {
                    new (&GetImg(Counters.NumImgs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::StorageTexelBuffer);
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER:
                case SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC:
                {
                    Uint32 Size = 0; // TODO
                    new (&GetUB(Counters.NumUBs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::UniformBuffer,
                                                   SPIRVShaderResourceAttribs::InvalidSepSmplrOrImgInd,
                                                   static_cast<Uint32>(Size));
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER:
                case SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC:
                {
                    auto ResType = binding->resource_type == SPV_REFLECT_RESOURCE_FLAG_SRV ?
                        SPIRVShaderResourceAttribs::ResourceType::ROStorageBuffer :
                        SPIRVShaderResourceAttribs::ResourceType::RWStorageBuffer;
                    const Uint32 Size   = 0;
                    const Uint32 Stride = 0; // TODO
                    new (&GetSB(Counters.NumSBs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   ResType,
                                                   SPIRVShaderResourceAttribs::InvalidSepSmplrOrImgInd,
                                                   static_cast<Uint32>(Size),
                                                   static_cast<Uint32>(Stride));
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_INPUT_ATTACHMENT:
                {
                    new (&GetInptAtt(Counters.NumInptAtts++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::InputAttachment);
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR:
                {
                    new (&GetAccelStruct(Counters.NumAccelStructs++))
                        SPIRVShaderResourceAttribs(*binding,
                                                   ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource)),
                                                   SPIRVShaderResourceAttribs::ResourceType::AccelerationStructure);
                    break;
                }
                case SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLED_IMAGE: break;
                default: UNEXPECTED("Unknown descriptor type");
            }
        }
    }

    for (uint32_t ds = 0; ds < pReflection->descriptor_set_count; ++ds)
    {
        const SpvReflectDescriptorSet& descSet = pReflection->descriptor_sets[ds];
        for (uint32_t b = 0; b < descSet.binding_count; ++b)
        {
            const SpvReflectDescriptorBinding* binding = descSet.bindings[b];
            if (binding == nullptr || binding->descriptor_type != SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLED_IMAGE)
                continue;

            Char*  name       = ResourceNamesPool.CopyString(GetResourceName(binding, m_IsHLSLSource));
            Uint32 SamplerInd = SPIRVShaderResourceAttribs::InvalidSepSmplrOrImgInd;

            if (CombinedSamplerSuffix != nullptr)
            {
                auto NumSepSmpls = GetNumSepSmplrs();
                for (SamplerInd = 0; SamplerInd < NumSepSmpls; ++SamplerInd)
                {
                    auto& SepSmplr = GetSepSmplr(SamplerInd);
                    if (StreqSuff(SepSmplr.Name, name, CombinedSamplerSuffix))
                    {
                        SepSmplr.AssignSeparateImage(Counters.NumSepImgs);
                        break;
                    }
                }
                if (SamplerInd == NumSepSmpls)
                    SamplerInd = SPIRVShaderResourceAttribs::InvalidSepSmplrOrImgInd;
            }
            auto* pNewSepImg = new (&GetSepImg(Counters.NumSepImgs++))
                SPIRVShaderResourceAttribs(*binding,
                                           name,
                                           SPIRVShaderResourceAttribs::ResourceType::SeparateImage,
                                           SamplerInd);
            if (pNewSepImg->IsValidSepSamplerAssigned())
            {
#ifdef DILIGENT_DEVELOPMENT
                const auto& SepSmplr = GetSepSmplr(pNewSepImg->GetAssignedSepSamplerInd());
                DEV_CHECK_ERR(SepSmplr.ArraySize == 1 || SepSmplr.ArraySize == pNewSepImg->ArraySize,
                              "Array size (", SepSmplr.ArraySize, ") of separate sampler variable '",
                              SepSmplr.Name, "' must be equal to 1 or be the same as the array size (", pNewSepImg->ArraySize,
                              ") of separate image variable '", pNewSepImg->Name, "' it is assigned to");
#endif
            }
        }
    }

    VERIFY_EXPR(Counters.NumUBs == ResCounters.NumUBs);
    VERIFY_EXPR(Counters.NumSBs == ResCounters.NumSBs);
    VERIFY_EXPR(Counters.NumImgs == ResCounters.NumImgs);
    VERIFY_EXPR(Counters.NumSmpldImgs == ResCounters.NumSmpldImgs);
    VERIFY_EXPR(Counters.NumSepSmplrs == ResCounters.NumSepSmplrs);
    VERIFY_EXPR(Counters.NumSepImgs == ResCounters.NumSepImgs);
    VERIFY_EXPR(Counters.NumInptAtts == ResCounters.NumInptAtts);
    VERIFY_EXPR(Counters.NumAccelStructs == ResCounters.NumAccelStructs);

    if (CombinedSamplerSuffix != nullptr)
    {
        m_CombinedSamplerSuffix = ResourceNamesPool.CopyString(CombinedSamplerSuffix);
    }

    m_ShaderName = ResourceNamesPool.CopyString(shaderDesc.Name);

    if (LoadShaderStageInputs)
    {
        Uint32 CurrStageInput = 0;
        for (uint32_t i = 0; i < pReflection->input_variable_count; ++i)
        {
            const SpvReflectInterfaceVariable* input = pReflection->input_variables[i];
            if (input == nullptr || input->built_in != -1)
                continue;

            new (&GetShaderStageInputAttribs(CurrStageInput++))
                SPIRVShaderStageInputAttribs(ResourceNamesPool.CopyString(input->semantic), input->word_offset.location);
        }
        VERIFY_EXPR(CurrStageInput == GetNumShaderStageInputs());
    }

    VERIFY(ResourceNamesPool.GetRemainingSize() == 0, "Names pool must be empty");

    //LOG_INFO_MESSAGE(DumpResources());

#ifdef DILIGENT_DEVELOPMENT
    if (CombinedSamplerSuffix != nullptr)
    {
        for (Uint32 n = 0; n < GetNumSepSmplrs(); ++n)
        {
            const auto& SepSmplr = GetSepSmplr(n);
            if (!SepSmplr.IsValidSepImageAssigned())
                LOG_ERROR_MESSAGE("Shader '", shaderDesc.Name, "' uses combined texture samplers, but separate sampler '", SepSmplr.Name, "' is not assigned to any texture");
        }
    }
#endif
}

void SPIRVShaderResources::Initialize(IMemoryAllocator&       Allocator,
                                      const ResourceCounters& Counters,
                                      Uint32                  NumShaderStageInputs,
                                      size_t                  ResourceNamesPoolSize,
                                      StringPool&             ResourceNamesPool)
{
    Uint32           CurrentOffset = 0;
    constexpr Uint32 MaxOffset     = std::numeric_limits<OffsetType>::max();
    auto             AdvanceOffset = [&CurrentOffset, MaxOffset](Uint32 NumResources) {
        VERIFY(CurrentOffset <= MaxOffset, "Current offset (", CurrentOffset, ") exceeds max allowed value (", MaxOffset, ")");
        (void)MaxOffset;
        auto Offset = static_cast<OffsetType>(CurrentOffset);
        CurrentOffset += NumResources;
        return Offset;
    };

    auto UniformBufferOffset = AdvanceOffset(Counters.NumUBs);
    (void)UniformBufferOffset;
    m_StorageBufferOffset   = AdvanceOffset(Counters.NumSBs);
    m_StorageImageOffset    = AdvanceOffset(Counters.NumImgs);
    m_SampledImageOffset    = AdvanceOffset(Counters.NumSmpldImgs);
    m_SeparateSamplerOffset = AdvanceOffset(Counters.NumSepSmplrs);
    m_SeparateImageOffset   = AdvanceOffset(Counters.NumSepImgs);
    m_InputAttachmentOffset = AdvanceOffset(Counters.NumInptAtts);
    m_AccelStructOffset     = AdvanceOffset(Counters.NumAccelStructs);
    m_TotalResources        = AdvanceOffset(0);
    static_assert(Uint32{SPIRVShaderResourceAttribs::ResourceType::NumResourceTypes} == 12, "Please update the new resource type offset");

    VERIFY(NumShaderStageInputs <= MaxOffset, "Max offset exceeded");
    m_NumShaderStageInputs = static_cast<OffsetType>(NumShaderStageInputs);

    auto AlignedResourceNamesPoolSize = Align(ResourceNamesPoolSize, sizeof(void*));

    static_assert(sizeof(SPIRVShaderResourceAttribs) % sizeof(void*) == 0, "Size of SPIRVShaderResourceAttribs struct must be multiple of sizeof(void*)");
    // clang-format off
    auto MemorySize = m_TotalResources              * sizeof(SPIRVShaderResourceAttribs) + 
                      m_NumShaderStageInputs        * sizeof(SPIRVShaderStageInputAttribs) +
                      AlignedResourceNamesPoolSize  * sizeof(char);

    VERIFY_EXPR(GetNumUBs()          == Counters.NumUBs);
    VERIFY_EXPR(GetNumSBs()          == Counters.NumSBs);
    VERIFY_EXPR(GetNumImgs()         == Counters.NumImgs);
    VERIFY_EXPR(GetNumSmpldImgs()    == Counters.NumSmpldImgs);
    VERIFY_EXPR(GetNumSepSmplrs()    == Counters.NumSepSmplrs);
    VERIFY_EXPR(GetNumSepImgs()      == Counters.NumSepImgs);
    VERIFY_EXPR(GetNumInptAtts()     == Counters.NumInptAtts);
    VERIFY_EXPR(GetNumAccelStructs() == Counters.NumAccelStructs);
    static_assert(Uint32{SPIRVShaderResourceAttribs::ResourceType::NumResourceTypes} == 12, "Please update the new resource count verification");
    // clang-format on

    if (MemorySize)
    {
        auto* pRawMem   = Allocator.Allocate(MemorySize, "Memory for shader resources", __FILE__, __LINE__);
        m_MemoryBuffer  = std::unique_ptr<void, STDDeleterRawMem<void>>(pRawMem, Allocator);
        char* NamesPool = reinterpret_cast<char*>(m_MemoryBuffer.get()) +
            m_TotalResources * sizeof(SPIRVShaderResourceAttribs) +
            m_NumShaderStageInputs * sizeof(SPIRVShaderStageInputAttribs);
        ResourceNamesPool.AssignMemory(NamesPool, ResourceNamesPoolSize);
    }
}

SPIRVShaderResources::~SPIRVShaderResources()
{
    for (Uint32 n = 0; n < GetNumUBs(); ++n)
        GetUB(n).~SPIRVShaderResourceAttribs();

    for (Uint32 n = 0; n < GetNumSBs(); ++n)
        GetSB(n).~SPIRVShaderResourceAttribs();

    for (Uint32 n = 0; n < GetNumImgs(); ++n)
        GetImg(n).~SPIRVShaderResourceAttribs();

    for (Uint32 n = 0; n < GetNumSmpldImgs(); ++n)
        GetSmpldImg(n).~SPIRVShaderResourceAttribs();

    for (Uint32 n = 0; n < GetNumSepSmplrs(); ++n)
        GetSepSmplr(n).~SPIRVShaderResourceAttribs();

    for (Uint32 n = 0; n < GetNumSepImgs(); ++n)
        GetSepImg(n).~SPIRVShaderResourceAttribs();

    for (Uint32 n = 0; n < GetNumInptAtts(); ++n)
        GetInptAtt(n).~SPIRVShaderResourceAttribs();

    for (Uint32 n = 0; n < GetNumShaderStageInputs(); ++n)
        GetShaderStageInputAttribs(n).~SPIRVShaderStageInputAttribs();

    for (Uint32 n = 0; n < GetNumAccelStructs(); ++n)
        GetAccelStruct(n).~SPIRVShaderResourceAttribs();

    static_assert(Uint32{SPIRVShaderResourceAttribs::ResourceType::NumResourceTypes} == 12, "Please add destructor for the new resource");
}



std::string SPIRVShaderResources::DumpResources()
{
    std::stringstream ss;
    ss << "Shader '" << m_ShaderName << "' resource stats: total resources: " << GetTotalResources() << ":" << std::endl
       << "UBs: " << GetNumUBs() << "; SBs: " << GetNumSBs() << "; Imgs: " << GetNumImgs() << "; Smpl Imgs: " << GetNumSmpldImgs()
       << "; Sep Imgs: " << GetNumSepImgs() << "; Sep Smpls: " << GetNumSepSmplrs() << '.' << std::endl
       << "Resources:";

    Uint32 ResNum       = 0;
    auto   DumpResource = [&ss, &ResNum](const SPIRVShaderResourceAttribs& Res) {
        std::stringstream FullResNameSS;
        FullResNameSS << '\'' << Res.Name;
        if (Res.ArraySize > 1)
            FullResNameSS << '[' << Res.ArraySize << ']';
        FullResNameSS << '\'';
        ss << std::setw(32) << FullResNameSS.str();

        if (Res.Type == SPIRVShaderResourceAttribs::ResourceType::SeparateImage && Res.IsValidSepSamplerAssigned())
        {
            ss << " Assigned sep sampler ind: " << Res.GetAssignedSepSamplerInd();
        }
        else if (Res.Type == SPIRVShaderResourceAttribs::ResourceType::SeparateSampler && Res.IsValidSepImageAssigned())
        {
            ss << " Assigned sep image ind: " << Res.GetAssignedSepImageInd();
        }

        ++ResNum;
    };

    ProcessResources(
        [&](const SPIRVShaderResourceAttribs& UB, Uint32) //
        {
            VERIFY(UB.Type == SPIRVShaderResourceAttribs::ResourceType::UniformBuffer, "Unexpected resource type");
            ss << std::endl
               << std::setw(3) << ResNum << " Uniform Buffer     ";
            DumpResource(UB);
        },
        [&](const SPIRVShaderResourceAttribs& SB, Uint32) //
        {
            VERIFY(SB.Type == SPIRVShaderResourceAttribs::ResourceType::ROStorageBuffer ||
                       SB.Type == SPIRVShaderResourceAttribs::ResourceType::RWStorageBuffer,
                   "Unexpected resource type");
            ss << std::endl
               << std::setw(3) << ResNum
               << (SB.Type == SPIRVShaderResourceAttribs::ResourceType::ROStorageBuffer ? " RO Storage Buffer  " : " RW Storage Buffer  ");
            DumpResource(SB);
        },
        [&](const SPIRVShaderResourceAttribs& Img, Uint32) //
        {
            if (Img.Type == SPIRVShaderResourceAttribs::ResourceType::StorageImage)
            {
                ss << std::endl
                   << std::setw(3) << ResNum << " Storage Image    ";
            }
            else if (Img.Type == SPIRVShaderResourceAttribs::ResourceType::StorageTexelBuffer)
            {
                ss << std::endl
                   << std::setw(3) << ResNum << " Storage Txl Buff ";
            }
            else
                UNEXPECTED("Unexpected resource type");
            DumpResource(Img);
        },
        [&](const SPIRVShaderResourceAttribs& SmplImg, Uint32) //
        {
            if (SmplImg.Type == SPIRVShaderResourceAttribs::ResourceType::SampledImage)
            {
                ss << std::endl
                   << std::setw(3) << ResNum << " Sampled Image    ";
            }
            else if (SmplImg.Type == SPIRVShaderResourceAttribs::ResourceType::UniformTexelBuffer)
            {
                ss << std::endl
                   << std::setw(3) << ResNum << " Uniform Txl Buff ";
            }
            else
                UNEXPECTED("Unexpected resource type");
            DumpResource(SmplImg);
        },
        [&](const SPIRVShaderResourceAttribs& SepSmpl, Uint32) //
        {
            VERIFY(SepSmpl.Type == SPIRVShaderResourceAttribs::ResourceType::SeparateSampler, "Unexpected resource type");
            ss << std::endl
               << std::setw(3) << ResNum << " Separate Smpl    ";
            DumpResource(SepSmpl);
        },
        [&](const SPIRVShaderResourceAttribs& SepImg, Uint32) //
        {
            VERIFY(SepImg.Type == SPIRVShaderResourceAttribs::ResourceType::SeparateImage, "Unexpected resource type");
            ss << std::endl
               << std::setw(3) << ResNum << " Separate Img     ";
            DumpResource(SepImg);
        },
        [&](const SPIRVShaderResourceAttribs& InptAtt, Uint32) //
        {
            VERIFY(InptAtt.Type == SPIRVShaderResourceAttribs::ResourceType::InputAttachment, "Unexpected resource type");
            ss << std::endl
               << std::setw(3) << ResNum << " Input Attachment ";
            DumpResource(InptAtt);
        },
        [&](const SPIRVShaderResourceAttribs& AccelStruct, Uint32) //
        {
            VERIFY(AccelStruct.Type == SPIRVShaderResourceAttribs::ResourceType::AccelerationStructure, "Unexpected resource type");
            ss << std::endl
               << std::setw(3) << ResNum << " Accel Struct     ";
            DumpResource(AccelStruct);
        } //
    );
    VERIFY_EXPR(ResNum == GetTotalResources());

    return ss.str();
}



bool SPIRVShaderResources::IsCompatibleWith(const SPIRVShaderResources& Resources) const
{
    // clang-format off
    if( GetNumUBs()               != Resources.GetNumUBs()        ||
        GetNumSBs()               != Resources.GetNumSBs()        ||
        GetNumImgs()              != Resources.GetNumImgs()       ||
        GetNumSmpldImgs()         != Resources.GetNumSmpldImgs()  ||
        GetNumSepImgs()           != Resources.GetNumSepImgs()    ||
        GetNumSepSmplrs()         != Resources.GetNumSepSmplrs()  ||
        GetNumInptAtts()          != Resources.GetNumInptAtts()   ||
        GetNumAccelStructs()      != Resources.GetNumAccelStructs())
        return false;
    // clang-format on
    VERIFY_EXPR(GetTotalResources() == Resources.GetTotalResources());
    static_assert(Uint32{SPIRVShaderResourceAttribs::ResourceType::NumResourceTypes} == 12, "Please update comparison with the new resource");

    bool IsCompatible = true;
    ProcessResources(
        [&](const SPIRVShaderResourceAttribs& Res, Uint32 n) {
            const auto& Res2 = Resources.GetResource(n);
            if (!Res.IsCompatibleWith(Res2))
                IsCompatible = false;
        });

    return IsCompatible;
}

} // namespace Diligent
