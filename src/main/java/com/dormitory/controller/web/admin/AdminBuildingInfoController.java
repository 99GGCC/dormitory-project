package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.BuildingInfoDTO;
import com.dormitory.controller.qry.BuildingInfoQry;
import com.dormitory.controller.vo.BuildingInfoVO;
import com.dormitory.controller.vo.ClassesInfoVO;
import com.dormitory.service.BuildingInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 楼栋信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/building/info")
@Api(value = "AdminBuildingInfoController", tags = {"楼栋信息接口"})
public class AdminBuildingInfoController {

    /**
     * 楼栋信息Service
     */
    private final BuildingInfoService buildingInfoService;

    @ApiOperation("楼栋信息分页查询")
    @GetMapping("/page")
    public R<IPage<BuildingInfoVO>> pageByQry(BuildingInfoQry qry) {
        return R.success(buildingInfoService.pageByQry(qry));
    }

    @ApiOperation("楼栋信息详情")
    @GetMapping("/detail/{buildingId}")
    public R<BuildingInfoVO> detail(@PathVariable @NotNull(message = "楼栋ID") Long buildingId) {
        return R.success(buildingInfoService.detailById(buildingId));
    }

    @ApiOperation("新增楼栋信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid BuildingInfoDTO dto) {
        return R.success(buildingInfoService.add(dto));
    }

    @ApiOperation("编辑楼栋信息")
    @PostMapping("/edit/{buildingId}")
    public R<Boolean> edit(@NotNull(message = "楼栋ID") @PathVariable Long buildingId,
                           @RequestBody @Valid BuildingInfoDTO dto) {
        return R.success(buildingInfoService.edit(buildingId, dto));
    }

    @ApiOperation("删除楼栋信息")
    @PostMapping("/del/{buildingId}")
    public R<Boolean> del(@NotNull(message = "楼栋ID") @PathVariable Long buildingId) {
        return R.success(buildingInfoService.del(buildingId));
    }
}

