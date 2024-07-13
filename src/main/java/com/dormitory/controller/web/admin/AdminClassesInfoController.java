package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.ClassesInfoDTO;
import com.dormitory.controller.qry.ClassesInfoQry;
import com.dormitory.controller.vo.ClassesInfoVO;
import com.dormitory.service.ClassesInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 班级信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/classes/info")
@Api(value = "AdminClassesInfoController", tags = {"班级信息接口"})
public class AdminClassesInfoController {
    /**
     * 班级信息Service
     */
    private final ClassesInfoService classesInfoService;

    @ApiOperation("班级信息分页查询")
    @GetMapping("/page")
    public R<IPage<ClassesInfoVO>> pageByQry(ClassesInfoQry qry) {
        return R.success(classesInfoService.pageByQry(qry));
    }

    @ApiOperation("班级信息详情")
    @GetMapping("/detail/{classesId}")
    public R<ClassesInfoVO> detail(@PathVariable @NotNull(message = "班级ID") Long classesId) {
        return R.success(classesInfoService.detailById(classesId));
    }

    @ApiOperation("新增班级信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid ClassesInfoDTO dto) {
        return R.success(classesInfoService.add(dto));
    }

    @ApiOperation("编辑班级信息")
    @PostMapping("/edit/{classesId}")
    public R<Boolean> edit(@NotNull(message = "班级ID") @PathVariable Long classesId,
                           @RequestBody @Valid ClassesInfoDTO dto) {
        return R.success(classesInfoService.edit(classesId, dto));
    }

    @ApiOperation("删除班级信息")
    @PostMapping("/del/{classesId}")
    public R<Boolean> del(@NotNull(message = "班级ID") @PathVariable Long classesId) {
        return R.success(classesInfoService.del(classesId));
    }
}

