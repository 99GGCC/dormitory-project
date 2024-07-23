package com.dormitory.controller.web.admin;


import com.dormitory.common.R;
import com.dormitory.controller.dto.BedInfoDTO;
import com.dormitory.service.BedInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 床位信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/bed/info")
@Api(value = "AdminBedInfoController", tags = {"床位信息接口"})
public class AdminBedInfoController {

    /**
     * 床位信息Service
     */
    private final BedInfoService bedInfoService;

    @ApiOperation("新增床位信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid BedInfoDTO dto) {
        return R.success(bedInfoService.add(dto));
    }

    @ApiOperation("编辑床位信息")
    @PostMapping("/edit/{BedId}")
    public R<Boolean> edit(@NotNull(message = "床位ID") @PathVariable Long BedId,
                           @RequestBody @Valid BedInfoDTO dto) {
        return R.success(bedInfoService.edit(BedId, dto));
    }

    @ApiOperation("删除床位信息")
    @PostMapping("/del/{BedId}")
    public R<Boolean> del(@NotNull(message = "床位ID") @PathVariable Long BedId) {
        return R.success(bedInfoService.del(BedId));
    }
}

