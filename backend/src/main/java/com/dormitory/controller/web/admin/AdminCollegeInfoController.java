package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.CollegeInfoDTO;
import com.dormitory.controller.qry.CollegeInfoQry;
import com.dormitory.controller.vo.CollegeInfoVO;
import com.dormitory.service.CollegeInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 学院信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/college/info")
@Api(value = "AdminCollegeInfoController", tags = {"学院信息接口"})
public class AdminCollegeInfoController {

    /**
     * 学院信息Service
     */
    private final CollegeInfoService collegeInfoService;

    @ApiOperation("学院信息分页查询")
    @GetMapping("/page")
    public R<IPage<CollegeInfoVO>> pageByQry(CollegeInfoQry qry) {
        return R.success(collegeInfoService.pageByQry(qry));
    }

    @ApiOperation("学院信息详情")
    @GetMapping("/detail/{collegeId}")
    public R<CollegeInfoVO> detail(@PathVariable @NotNull(message = "学院ID") Long collegeId) {
        return R.success(collegeInfoService.detailById(collegeId));
    }

    @ApiOperation("新增学院信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid CollegeInfoDTO dto) {
        return R.success(collegeInfoService.add(dto));
    }

    @ApiOperation("编辑学院信息")
    @PostMapping("/edit/{collegeId}")
    public R<Boolean> edit(@NotNull(message = "学院ID") @PathVariable Long collegeId,
                           @RequestBody @Valid CollegeInfoDTO dto) {
        return R.success(collegeInfoService.edit(collegeId, dto));
    }

    @ApiOperation("删除学院信息")
    @PostMapping("/del/{collegeId}")
    public R<Boolean> del(@NotNull(message = "学院ID") @PathVariable Long collegeId) {
        return R.success(collegeInfoService.del(collegeId));
    }
}

