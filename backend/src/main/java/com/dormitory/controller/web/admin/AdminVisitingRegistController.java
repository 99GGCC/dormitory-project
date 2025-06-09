package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.VisitingRegistDTO;
import com.dormitory.controller.qry.VisitingRegistQry;
import com.dormitory.controller.vo.VisitingRegistVO;
import com.dormitory.service.VisitingRegistService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 来访登记表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/visiting/regist")
@Api(value = "AdminVisitingRegistController", tags = {"来访登记接口"})
public class AdminVisitingRegistController {

    /**
     * 来访登记Service
     */
    private final VisitingRegistService visitingRegistService;

    @ApiOperation("来访登记分页查询")
    @GetMapping("/page")
    public R<IPage<VisitingRegistVO>> pageByQry(VisitingRegistQry qry) {
        return R.success(visitingRegistService.pageByQry(qry));
    }

    @ApiOperation("来访登记详情")
    @GetMapping("/detail/{registId}")
    public R<VisitingRegistVO> detail(@PathVariable @NotNull(message = "登记ID") Long registId) {
        return R.success(visitingRegistService.detailById(registId));
    }

    @ApiOperation("新增来访登记信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid VisitingRegistDTO dto) {
        return R.success(visitingRegistService.add(dto));
    }

    @ApiOperation("编辑来访登记信息")
    @PostMapping("/edit/{registId}")
    public R<Boolean> edit(@NotNull(message = "来访登记ID") @PathVariable Long registId,
                           @RequestBody @Valid VisitingRegistDTO dto) {
        return R.success(visitingRegistService.edit(registId, dto));
    }

    @ApiOperation("删除来访登记信息")
    @PostMapping("/del/{registId}")
    public R<Boolean> del(@NotNull(message = "来访登记ID") @PathVariable Long registId) {
        return R.success(visitingRegistService.del(registId));
    }
}

