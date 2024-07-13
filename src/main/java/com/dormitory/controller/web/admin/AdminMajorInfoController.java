package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.MajorInfoDTO;
import com.dormitory.controller.qry.MajorInfoQry;
import com.dormitory.controller.vo.MajorInfoVO;
import com.dormitory.service.MajorInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 专业信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/major/info")
@Api(value = "AdminMajorInfoController", tags = {"专业信息接口"})
public class AdminMajorInfoController {
    /**
     * 专业信息Service
     */
    private final MajorInfoService majorInfoService;

    @ApiOperation("专业信息分页查询")
    @GetMapping("/page")
    public R<IPage<MajorInfoVO>> pageByQry(MajorInfoQry qry) {
        return R.success(majorInfoService.pageByQry(qry));
    }

    @ApiOperation("专业信息详情")
    @GetMapping("/detail/{majorId}")
    public R<MajorInfoVO> detail(@PathVariable @NotNull(message = "专业ID") Long majorId) {
        return R.success(majorInfoService.detailById(majorId));
    }

    @ApiOperation("新增专业信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid MajorInfoDTO dto) {
        return R.success(majorInfoService.add(dto));
    }

    @ApiOperation("编辑专业信息")
    @PostMapping("/edit/{majorId}")
    public R<Boolean> edit(@NotNull(message = "专业ID") @PathVariable Long majorId,
                           @RequestBody @Valid MajorInfoDTO dto) {
        return R.success(majorInfoService.edit(majorId, dto));
    }

    @ApiOperation("删除专业信息")
    @PostMapping("/del/{majorId}")
    public R<Boolean> del(@NotNull(message = "专业ID") @PathVariable Long majorId) {
        return R.success(majorInfoService.del(majorId));
    }
}

